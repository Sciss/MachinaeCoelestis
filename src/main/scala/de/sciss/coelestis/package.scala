package de.sciss

import _root_.play.api.libs.json.{JsSuccess, JsError, JsNumber, JsObject, JsResult, JsValue, Format}
import de.sciss.file._
import de.sciss.mellite.Document
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

  lazy val sessionFile  = userHome / "Desktop" / "MachinaeCoelestis" / "mellite" / "MachinaeCoelestis.mllt"

  lazy val firstDate    = "2013-08-16 17:10:27".toDate
  lazy val lastDate     = "2013-08-27 01:44:54".toDate
  lazy val firstDateT   = firstDate.getTime
  lazy val lastDateT    = lastDate.getTime

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
          val p = prog.toInt * barLength/100
          while (lastProg < p) {
            print('#')
            lastProg += 1
          }
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
}