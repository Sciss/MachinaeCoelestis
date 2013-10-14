package de.sciss.coelestis

import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.lucre.confluent
import scala.concurrent._
import de.sciss.mellite.{ProcActions, Element}
import de.sciss.span.Span
import de.sciss.synth.proc.{FadeSpec, ProcKeys, Attribute}
import de.sciss.lucre.confluent.VersionPeek
import de.sciss.file._
import de.sciss.model.Change
import play.api.libs.json.{JsSuccess, JsNumber, JsError, JsObject, JsResult, JsValue, Format}
import de.sciss.play.json.{Formats, AutoFormat}
import scala.concurrent.duration.Duration

object RegionAnalysisLike {
  type Res = Vec[RegionCommand]

  implicit object SpanFormat extends Format[Span] {
    def reads(json: JsValue): JsResult[Span] = json match {
      case JsObject(fields) =>
        val map = fields.toMap
        map.get("start").fold[JsResult[Span]](JsError("Field 'start' missing")) {
          case JsNumber(start) =>
            map.get("stop").fold[JsResult[Span]](JsError("Field 'stop' missing")) {
              case JsNumber(stop) =>
                JsSuccess(Span(start.toLong, stop.toLong))
              case other => JsError(s"Not a JSON number: $other")
            }

          case other => JsError(s"Not a JSON number: $other")
        }
      case _ => JsError(s"Not a JSON object: $json")
    }

    def writes(s: Span): JsValue = JsObject(
        Seq("start" -> JsNumber(s.start), "stop" -> JsNumber(s.stop))
      )
  }

  case class Region(id: Int, time: Span, gain: Double, fades: (FadeSpec.Value, FadeSpec.Value),
                     muted: Boolean, kind: RegionKind) {

    override def toString = {
      val gainString  = if (gain == 1.0) "" else s", gain = $gain"
      val fadesString = if (fades == (NoFade, NoFade)) "" else s", $fades"
      val mutedString = if (muted) ", muted = true" else ""
      s"Region($id, $time$gainString$fadesString$mutedString, $kind)"
    }
  }

  case class AudioRegion(artifact: File) extends RegionKind
  case object FuncRegion extends RegionKind
  sealed trait RegionKind

  // case class RegionCommand(time: Time, region: Region, action: Action)
  case class RegionCommand(time: Time, region: Region, action: Action)

  val NoFade = FadeSpec.Value(0L)

  sealed trait ActionType {
    def name : String
  }
  sealed trait Action2 {
    def tpe: ActionType
    def actionSpan: Span
    def inputRegion: Region
  }
  object RegionAdded extends ActionType {
    def name = "Add"
  }
  sealed trait RegionAction extends Action2 {
    def r: Region
    def actionSpan  = r.time
    def inputRegion = r
  }
  case class RegionAdded  (r: Region) extends RegionAction {
    def tpe         = RegionAdded
  }
  object RegionRemoved extends ActionType {
    def name = "Remove"
  }
  case class RegionRemoved(r: Region) extends RegionAction {
    def tpe = RegionRemoved
  }
  case class RegionMutated(ch: Change[Region], mutation: Mutation) extends Action2 {
    def tpe         = mutation
    def actionSpan  = ch.before.time union ch.now.time
    def inputRegion = ch.before
  }
  object RegionSplit extends ActionType {
    def name = "Split"
  }
  case class RegionSplit(old: Change[Region], nu: Region) extends Action2 {
    def tpe         = RegionSplit
    def actionSpan  = old.before.time
    def inputRegion = old.before
  }

  sealed trait Mutation extends ActionType
  case object MoveChange   extends Mutation {
    def name = "Move"
  }
  case object ResizeChange extends Mutation {
    def name = "Resize"
  }
  case object GainChange   extends Mutation {
    def name = "Gain"
  }
  case object FadeChange   extends Mutation {
    def name = "Fade"
  }
  case object MuteChange   extends Mutation {
    def name = "Mute"
  }
  case object FileChange   extends Mutation {
    def name = "File"
  }

  case class TimedAction(time: Time, action: Action2)

  implicit val format: Format[Vec[RegionCommand]] = {
    import FadeSpec.Value
    implicit val fmtTime      = AutoFormat[Time]
    // implicit val fmtSpan      = AutoFormat[Span]
    implicit val fmtFade      = AutoFormat[Value]
    implicit val fmtFades     = Formats.Tuple2Format[Value, Value]
    implicit val fmtFile: Format[File] = Formats.FileFormat
    implicit val fmtRegionKind = AutoFormat[RegionKind]
    implicit val fmtRegion    = AutoFormat[Region]
    implicit val fmtRegionCmd = AutoFormat[RegionCommand]
    AutoFormat[Vec[RegionCommand]]
  }
}
trait RegionAnalysisLike extends AnalysisLike {
  import RegionAnalysisLike._

  val skip = 200L  // milliseconds steps

  // lazy val jsonFile = analysisDir / "regions.json"

  // lazy val jsonFile = analysisDir / "regions2.json"

  // def jsonFile: File

  protected def specificHistory(name: String, iteration: Int = -1): Vec[TimedAction] =
    if (name == "Indeterminus" && iteration < 0) {
      (0 to 4).flatMap { iter =>
        val jsonFile = analysisDir / s"${name}_regions$iter.json"
        globalHistory(jsonFile)
      } .sortBy(_.time.version) // sortin not actually necessary for the below plot...
    } else {
      val jsonFile = analysisDir / s"${name}_regions${if (name == "Indeterminus") iteration.toString else ""}.json"
      globalHistory(jsonFile)
    }

  def generateJSON(jsonFile: File)(done: => Unit): Unit = {
    if (jsonFile.isFile) {
      println(s"File '$jsonFile' already generated.")
      done
    } else {
      val p = gather()
      // sucky executor spawns daemon threads
      new Thread {
        override def run(): Unit = {
          Await.ready(p, Duration.Inf)
          Thread.sleep(2000)
        }
        start()
      }
      p.monitor(barLength = 100)
      p.foreach { xs =>
        println(s"\nWriting '$jsonFile'...")
        JsIO.write(xs, jsonFile)
        // quit()
        done
      }
    }
  }

  def globalHistory(jsonFile: File): Vec[TimedAction] = {
    val data  = JsIO.read[Res](jsonFile).get
    // var state = Map.empty[Int, Region]

    var history = Vec.empty[TimedAction]

    val map   = data.groupBy(_.time.version).toIndexedSeq.sortBy(_._1)
    map.foreach { case (_, cmds) =>
      cmds.groupBy(_.region.id).foreach { case (_, sq) =>
        val action = sq match {
          case Vec(single) =>
            single.action match {
              case Added    => RegionAdded  (single.region)
              case Removed  => RegionRemoved(single.region)
            }

          case Vec(a, b) =>
            val (old, nu) = if (a.action == Removed && b.action == Added  ) (a.region, b.region)
            else            if (a.action == Added   && b.action == Removed) (b.region, a.region)
            else sys.error("Unexpected")

            val mutation = if (old.time != nu.time) {
              if (old.time.start == nu.time.start || old.time.stop == nu.time.stop) ResizeChange
              else MoveChange
            } else if (old.gain  != nu.gain ) GainChange
            else   if (old.fades != nu.fades) FadeChange
            else   if (old.muted != nu.muted) MuteChange
            else   if (old.kind  != nu.kind ) FileChange
            else sys.error("Unexpected")

            RegionMutated(Change(old, nu), mutation)
        }
        val time = sq.head.time
        // heuristically determine split and two-step resize
        (history, action) match {
          case (init :+ TimedAction(`time`,
            RegionMutated(Change(rOld @ Region(_, span1Old, _, _, _, _), rOld2 @ Region(_, span1, _, _, _, _)), ResizeChange)),
              RegionAdded(rNew @ Region(_, span2, _, _, _, _)))
            if span1Old.start == span1.start && span1.stop == span2.start && span1Old.stop == span2.stop =>

              history = init :+ TimedAction(time, RegionSplit(Change(rOld, rOld2), rNew))

          case (init :+ TimedAction(timeOld, RegionSplit(Change(rOld, rOld2), rNu)), RegionRemoved(rRem)) if rRem == rNu || rRem == rOld2 =>
              history = init :+ TimedAction(timeOld,
                RegionMutated(Change(rOld, if (rRem == rNu) rOld2 else rNu), ResizeChange))

          case _ =>

              history :+= TimedAction(time, action)
        }
      }
    }

    history // .foreach(println)
  }

  def gather(): Processor[Res, Any] = {
    val csr = analysisCursor

    val proc = new ProcessorImpl[Res, Any] {
      def body(): Res = {
        var t           = firstDateT
        var predPath: S#Acc = confluent.Sys.Acc.root[S]
        var pred        = Set.empty[Region]
        var res         = Vec.empty[RegionCommand]

        while (t < /* firstDateT + (10L*60*60*1000) */ lastDateT) {
          blocking {
            val (t1, info, p) = masterCursor.step { implicit tx => findNextVersion(t, predPath, skip) }
            val regions = csr.stepFrom(p) { implicit tx =>
              val groupOption = session.collectElements {
                case e: Element.ProcGroup[S] if e.name.value == "Timeline" => e.entity
              } .headOption // .getOrElse(sys.error("Did not find timeline named 'Timeline'"))

              groupOption.fold(Vec.empty[Region]) { group =>
                group.iterator.toIndexedSeq.flatMap {
                  case (span @ Span(_, _), elems) =>
                    elems.map { timed =>
                      val proc    = timed.value
                      val gain    = proc.attributes[Attribute.Double  ](ProcKeys.attrGain   ).map(_.value).getOrElse(1.0)
                      val muted   = proc.attributes[Attribute.Boolean ](ProcKeys.attrMute   ).exists(_.value)
                      val fadeIn  = proc.attributes[Attribute.FadeSpec](ProcKeys.attrFadeIn ).map(_.value).getOrElse(NoFade)
                      val fadeOut = proc.attributes[Attribute.FadeSpec](ProcKeys.attrFadeOut).map(_.value).getOrElse(NoFade)
                      val fades   = (fadeIn, fadeOut)
                      val kind    = ProcActions.getAudioRegion(timed.span, proc) match {
                        case Some((_, audio)) => AudioRegion(audio.artifact.value)
                        case _ => FuncRegion
                      }
                      Region(id = timed.id.base, time = span, gain = gain, fades = fades, muted = muted, kind = kind)
                    }
                  case _ => Vec.empty
                }
              }
            }
            val succ  = regions.toSet

            t         = t1
            predPath  = p
            if (succ != pred) {
              val added     = succ -- pred
              val removed   = pred -- succ

              // val pDate = new Date(info.timeStamp)
              // println(s"At ${dateFormat.format(pDate)} added $added, removed $removed")

              val time = Time(stamp = info.timeStamp, version = VersionPeek(p))

              added.foreach { g =>
                res :+= RegionCommand(time, g, Added)
              }
              removed.foreach { g =>
                res :+= RegionCommand(time, g, Removed)
              }
              pred = succ
            }
          }
          progress((t - firstDateT).toFloat / (lastDateT - firstDateT).toFloat)
        }
        progress(1f)
        res
      }
    }
    proc.start()
    proc
  }
}
