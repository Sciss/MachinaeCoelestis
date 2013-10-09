package de.sciss

import de.sciss.file._
import de.sciss.mellite.Document
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import de.sciss.synth.proc.Confluent
import scala.concurrent.ExecutionContext
import de.sciss.processor.Processor
import scala.util.Failure
import scala.util.Success

package object coelestis {
  type S = Confluent
  type D = S#D

  lazy val sessionFile  = userHome / "Desktop" / "MachinaeCoelestis" / "mellite" / "MachinaeCoelestis.mllt"

  lazy val firstDate    = "2013-08-16 17:10:27".toDate
  lazy val lastDate     = "2013-08-27 01:44:54".toDate

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

  case class AudioFileInfo(path: String, length: Long, numChannels: Int)

  case class Command(time: Time, info: AudioFileInfo, action: Action)

  val  Vec      = collection.immutable.IndexedSeq
  type Vec[+A]  = collection.immutable.IndexedSeq[A]

  implicit def executionContext: ExecutionContext = ExecutionContext.global

  implicit class RichProcessor(proc: Processor[Any, _]) {
    def monitor(printResult: Boolean = false): Unit = {
      var lastProg = 0
      proc.addListener {
        case prog @ Processor.Progress(_, _) =>
          val p = prog.toInt/3
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
}