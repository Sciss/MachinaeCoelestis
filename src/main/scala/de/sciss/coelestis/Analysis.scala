package de.sciss.coelestis

import de.sciss.mellite.Element
import de.sciss.synth.proc.Grapheme
import java.util.Date
import de.sciss.lucre.confluent
import de.sciss.lucre.confluent.{VersionInfo, Cursor}
import de.sciss.lucre.synth.expr.Strings
import scala.annotation.tailrec

object Analysis extends App {
  val skip = 4000L  // milliseconds steps

  audioFiles()

  def audioFiles(): Unit = {
    val masterCursor = session.cursors.cursor
    val csr: Cursor[S, D] = masterCursor.step { implicit tx =>
      implicit val dtx: D#Tx = tx
      val existing = session.cursors.descendants.toList.collectFirst {
        case c if c.name.value == AnalysisCursor => c.cursor
      }
      existing.getOrElse {
        val cNew  = session.cursors.addChild(masterCursor.position)
        cNew.name = Strings.newVar[D](Strings.newConst(AnalysisCursor))
        cNew.cursor
      }
    }

    val firstDateT  = firstDate.getTime
    val lastDateT   = lastDate.getTime
    var t           = firstDateT
    var predPath: S#Acc = confluent.Sys.Acc.root[S]
    var pred        = Set.empty[Grapheme.Value.Audio]
    var reportTime  = 0L

    def report(): Unit = {
      println(s"${dateFormat.format(new Date(t))} - $predPath")
      reportTime = System.currentTimeMillis()
    }

    while (t < lastDateT) {
      def findFilesAt(): (S#Acc, VersionInfo, Set[Grapheme.Value.Audio]) = {
        val (path, info)  = csr.step { implicit tx =>
          implicit val dtx: D#Tx = tx
          @tailrec def loop(): S#Acc = {
            val p1 = masterCursor.position.takeUntil(t)
            if (p1 == predPath) {
              t += skip
              loop()
            } else p1
          }
          val res = loop()
          (res, res.info)
        }
        val set = csr.stepFrom(path) { implicit tx =>
          session.collectElements {
            case e: Element.AudioGrapheme[S] => e.entity.value
          } .toSet
        }
        (path, info, set)
      }

      val (p, info, succ) = findFilesAt()
      predPath = p
      if (succ != pred) {
        val added     = succ -- pred
        val removed   = pred -- succ
        // report()
        val pDate = new Date(info.timeStamp)
        println(s"At ${dateFormat.format(pDate)} added $added, removed $removed")
        pred = succ
      } else {
        val elapsed = System.currentTimeMillis() - reportTime
        if (elapsed >= 10000) report()
        //        if (elapsed >= 60000) {
        //          println("Continue [YES, no] ?")
        //          if (Console.readLine().headOption.map(_.toUpper) == Some('N')) quit()
        //        }

      }
      t += skip // skip four seconds
    }

    quit()
  }

  def quit(): Unit = {
    println("Closing...")
    session.system.close()
    sys.exit()
  }
}
