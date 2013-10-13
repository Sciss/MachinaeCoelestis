package de.sciss

import _root_.play.api.libs.json.{JsSuccess, JsError, JsNumber, JsObject, JsResult, JsValue, Format}
import de.sciss.file._
import de.sciss.mellite.{Folder, ConfluentDocument, Element, Document}
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import de.sciss.synth.proc.Confluent
import scala.concurrent.ExecutionContext
import de.sciss.processor.Processor
import scala.util.Failure
import scala.util.Success
import scala.concurrent.stm.Txn
import de.sciss.synth.Curve

package object coelestis {
  type S = Confluent
  type D = S#D

  def sessionName = "Indeterminus"

  private lazy val desktop = userHome / "Desktop"
  // lazy val sessionFile  = desktop / "MachinaeCoelestis" / "mellite" / "MachinaeCoelestis.mllt"
  lazy val sessionFile  = desktop / "Indeterminus" / s"$sessionName.mllt"

  // lazy val iteration    = 0

  case class Meta(iteration: Int) {
    val timelineName = s"Mechanik${if (iteration > 0) "_it"+iteration else ""}" // "Timeline"

    // on May 18th ProcImpl serialization was revised. There is incompatible material in the session
    // file. I suppose the actual production doesn't start yet before that date.
    // lazy val firstDate    = "2013-05-15 18:14:25".toDate
    val firstDate = {
      val ref = if (iteration > 0) "2013-05-15 18:14:25" else "2013-05-18 23:00:00" // between 22 and 23 h
      ref.toDate
    }
    val lastDate     = {
      val ref = if (iteration > 0) "2013-05-30 00:03:18" else "2013-05-23 00:00:00"
      ref.toDate
    }
    val firstDateT   = firstDate.getTime
    val lastDateT    = lastDate .getTime

    val regionJsonFile = analysisDir / s"${sessionName}_regions$iteration.json"
  }

  private val sync = new AnyRef
  private var _sessionOpen = false

  def sessionOpen: Boolean = _sessionOpen

  lazy val session      = {
    sync.synchronized(_sessionOpen = true)
    Document.read(sessionFile)
  }

  lazy val dateFormat   = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.US)

  lazy val analysisDir  = new File("analysis")

  implicit class RichString(s: String) {
    def toDate: Date = dateFormat.parse(s)
  }

  val AnalysisCursor = "Analysis"

  case class Time(stamp: Long, version: Int) {
    lazy val date = new Date(stamp)

    override def toString = s"$productPrefix($date, version = $version)"
  }

  val  Vec      = collection.immutable.IndexedSeq
  type Vec[+A]  = collection.immutable.IndexedSeq[A]

  implicit def executionContext: ExecutionContext = ExecutionContext.global

  implicit class RichProcessor(proc: Processor[Any, _]) {
    def monitor(printResult: Boolean = false, barLength: Int = 50): Unit = {
      var lastProg = 0
      proc.addListener {
        case prog @ Processor.Progress(_, _) =>
          val p = prog.toInt
          if (p != lastProg) {
            println(s"$p%")
            lastProg = p
          }
        //          val p = prog.toInt * barLength/100
        //          while (lastProg < p) {
        //            Console.print('#')
        //            Console.flush()
        //            lastProg += 1
        //          }
      }

      proc.onComplete {
        case Failure(Processor.Aborted()) =>
          println(s" Aborted $proc")

        case Failure(err) =>
          println(s" Failure: $proc")
          err.printStackTrace()

        case Success(_) =>
          println(if (printResult) s" Result: $proc" else " Ok.")
      }
    }
  }

  def requireNoTxn(): Unit = require(Txn.findCurrent.isEmpty, "Must be called outside of Txn")

  implicit object CurveFormat extends Format[Curve] {
    def reads(json: JsValue): JsResult[Curve] = json match {
      case JsObject(fields) =>
        val map = fields.toMap
        map.get("id") match {
          case Some(JsNumber(idj)) =>
            val id = idj.toInt
            id match {
              case Curve.parametric.id =>
                map.get("curvature") match {
                  case Some(JsNumber(x)) => JsSuccess(Curve.parametric(x.toFloat))
                  case _ => JsError(s"Field 'curvature' not found in $fields")
                }
              case Curve.step       .id => JsSuccess(Curve.step)
              case Curve.linear     .id => JsSuccess(Curve.linear)
              case Curve.exponential.id => JsSuccess(Curve.exponential)
              case Curve.sine       .id => JsSuccess(Curve.sine)
              case Curve.welch      .id => JsSuccess(Curve.welch)
              case Curve.squared    .id => JsSuccess(Curve.squared)
              case Curve.cubed      .id => JsSuccess(Curve.cubed)
              case other               => JsError(s"Unexpected envelope shape ID $other")
            }

          case _ => JsError(s"Field 'id' not found in $fields")
        }

      case other => JsError(s"Not a JSON object: $other")
    }

    def writes(c: Curve): JsValue = {
      val f0 = ("id" -> JsNumber(c.id)) :: Nil
      val f1 = c match {
        case Curve.parametric(x) => ("curvature" -> JsNumber(x)) :: f0
        case _ => f0
      }
      JsObject(f1)
    }
  }

  implicit class RichIterable[A](it: Iterable[A]) {
    def counted: Map[A, Int] = (Map.empty[A, Int].withDefaultValue(0) /: it)((m, e) => m.updated(e, m(e) + 1))
  }

  implicit class RichConfluentDocument(doc: ConfluentDocument) {
    def collectElements[A](pf: PartialFunction[Element[S], A])(implicit tx: S#Tx): Vec[A] = {
      var b   = Vec.newBuilder[A]
      val fun = pf.lift

      def loop(f: Folder[S]): Unit =
        f.iterator.foreach { elem =>
          fun(elem).foreach(b += _)
          elem match {
            case ef: Element.Folder[S] => loop(ef.entity)
            case _ =>
          }
        }

      loop(doc.elements)
      b.result()
    }
  }
}