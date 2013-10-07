package de.sciss.coelestis

import de.sciss.mellite.Element
import de.sciss.synth.proc.Grapheme
import java.util.Date
import de.sciss.lucre.confluent
import de.sciss.lucre.confluent.Cursor
import de.sciss.lucre.synth.expr.Strings

object Analysis extends App {
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

    def filesAt(time: Long): (S#Acc, Set[Grapheme.Value.Audio]) = {
      val path  = csr.step { implicit tx =>
        implicit val dtx: D#Tx = tx
        masterCursor.position.takeUntil(time)
      }
      val set = csr.stepFrom(path) { implicit tx =>
        session.collectElements {
          case e: Element.AudioGrapheme[S] => e.entity.value
        } .toSet
      }
      (path, set)
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
      val (p, succ) = filesAt(t)
      predPath = p
      if (succ != pred) {
        val added     = succ -- pred
        val removed   = pred -- succ
        report()
        println(s"added $added, removed $removed")
        pred = succ
      } else {
        val elapsed = System.currentTimeMillis() - reportTime
        if (elapsed >= 10000) report()
        //        if (elapsed >= 60000) {
        //          println("Continue [YES, no] ?")
        //          if (Console.readLine().headOption.map(_.toUpper) == Some('N')) quit()
        //        }

      }
      t += 40000 // skip four seconds
    }

    quit()
  }

  def quit(): Unit = {
    println("Closing...")
    session.system.close()
    sys.exit()
  }
}
