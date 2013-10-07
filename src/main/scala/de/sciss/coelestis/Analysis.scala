package de.sciss.coelestis

import de.sciss.mellite.Element
import de.sciss.synth.proc.Grapheme
import java.util.Date

object Analysis extends App {
  audioFiles()

  def audioFiles(): Unit = {
    val csr   = session.cursors.cursor

    def filesAt(time: Long): Set[Grapheme.Value.Audio] = {
      val path  = csr.step { implicit tx =>
        tx.inputAccess.takeUntil(time)
      }
      csr.stepFrom(path) { implicit tx =>
        session.collectElements {
          case e: Element.AudioGrapheme[S] => e.entity.value
        } .toSet
      }
    }

    val firstDateT  = firstDate.getTime
    val lastDateT   = lastDate.getTime
    var t           = firstDateT
    var pred        = Set.empty[Grapheme.Value.Audio]
    var reportTime  = 0L

    def report(): Unit = {
      println(dateFormat.format(new Date(t)))
      reportTime = System.currentTimeMillis()
    }

    while (t < lastDateT) {
      val succ  = filesAt(t)
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
      t += 4000 // skip four seconds
    }
  }

  def quit(): Unit = {
    session.system.close()
    sys.exit()
  }
}
