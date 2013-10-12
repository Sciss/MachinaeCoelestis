package de.sciss.coelestis

import de.sciss.processor.Processor
import de.sciss.mellite.{ProcActions, Element}
import de.sciss.span.Span
import de.sciss.lucre.confluent
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.lucre.confluent.VersionPeek
import de.sciss.file._
import scala.concurrent.{Await, blocking}
import scala.concurrent.duration.Duration
import play.api.libs.json.{JsSuccess, JsError, JsResult, JsNumber, JsObject, JsValue, Format}
import de.sciss.play.json.{Formats, AutoFormat}
import de.sciss.synth.proc.{Attribute, ProcKeys, FadeSpec}

object RegionAnalysis extends AnalysisLike {
  val skip = 200L  // milliseconds steps

  // case class Region(id: Int, time: Span)
  case class Region2(id: Int, time: Span, gain: Double, fades: (FadeSpec.Value, FadeSpec.Value),
                     muted: Boolean, kind: RegionKind)

  case class AudioRegion(artifact: File) extends RegionKind
  case object FuncRegion extends RegionKind
  sealed trait RegionKind

  // case class RegionCommand(time: Time, region: Region, action: Action)
  case class RegionCommand(time: Time, region: Region2, action: Action)

  val NoFade = FadeSpec.Value(0L)

  def apply(): Unit = {
    generateJSON(plot())
  }

  // lazy val jsonFile = analysisDir / "regions.json"
  lazy val jsonFile = analysisDir / "regions2.json"

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

  implicit def format: Format[Vec[RegionCommand]] = {
    import FadeSpec.Value
    implicit val fmtTime      = AutoFormat[Time]
    // implicit val fmtSpan      = AutoFormat[Span]
    implicit val fmtFade      = AutoFormat[Value]
    implicit val fmtFades     = Formats.Tuple2Format[Value, Value]
    implicit val fmtFile: Format[File] = Formats.FileFormat
    implicit val fmtRegionKind = AutoFormat[RegionKind]
    implicit val fmtRegion    = AutoFormat[Region2]
    implicit val fmtRegionCmd = AutoFormat[RegionCommand]
    AutoFormat[Vec[RegionCommand]]
  }

  def generateJSON(done: => Unit): Unit = {
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

  def plot(): Unit = {
    val data  = JsIO.read[Res](jsonFile).get
    val map   = data.groupBy(_.time.version)
    //    val rich  = map.mapValues { sq =>
    //
    //      def loop(in: Res, res: Res): Res = in match {
    //        case head +: tail =>
    //
    //
    //          ???
    //        case _ => res
    //      }
    //
    //    }
  }

  def gather(): Processor[Res, Any] = {
    val csr = analysisCursor

    val proc = new ProcessorImpl[Res, Any] {
      def body(): Res = {
        var t           = firstDateT
        var predPath: S#Acc = confluent.Sys.Acc.root[S]
        var pred        = Set.empty[Region2]
        var res         = Vec.empty[RegionCommand]

        while (t < /* firstDateT + (10L*60*60*1000) */ lastDateT) {
          blocking {
            val (t1, info, p) = masterCursor.step { implicit tx => findNextVersion(t, predPath, skip) }
            val regions = csr.stepFrom(p) { implicit tx =>
              val groupOption = session.collectElements {
                case e: Element.ProcGroup[S] if e.name.value == "Timeline" => e.entity
              } .headOption // .getOrElse(sys.error("Did not find timeline named 'Timeline'"))

              groupOption.fold(Vec.empty[Region2]) { group =>
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
                      Region2(id = timed.id.base, time = span, gain = gain, fades = fades, muted = muted, kind = kind)
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