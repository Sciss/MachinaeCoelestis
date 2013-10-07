package de.sciss.coelestis

import de.sciss.mellite.Element
import de.sciss.synth.proc.Grapheme
import java.util.Date
import de.sciss.lucre.confluent
import de.sciss.lucre.confluent.{VersionInfo, Cursor}
import de.sciss.lucre.synth.expr.Strings
import scala.annotation.tailrec

object Analysis extends App {
  val skip      = 4000L  // milliseconds steps
  val LOG_SKIP  = false
  val LOG_PROG  = true

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

    def report(): Unit = if (LOG_PROG) {
      println(s"${dateFormat.format(new Date(t))} - $predPath")
      reportTime = System.currentTimeMillis()
    }

    def findNextVersion(prevTime: Long, prevPath: S#Acc, minSkip: Long = 4000)
                       (implicit tx: S#Tx): (Long, VersionInfo, S#Acc) = {
      implicit val dtx: D#Tx = tx
      val p0 = masterCursor.position

      // find interval
      @tailrec def loopFwd(t: Long, sk: Long): (Long, Long, S#Acc) = {
        val t1 = math.min(lastDateT, t + sk)
        if (LOG_SKIP) println(s"Skip >> ${dateFormat.format(new Date(t1))}")
        val p1 = p0.takeUntil(t1)
        if (t1 < lastDateT && p1 == prevPath) {
          val sk1 = if (sk < 0x80000000L) sk << 1 else sk
          loopFwd(t1, sk1)
        } else {
          (t1, sk, p1)
        }
      }

      // either pf != predPath or tf == lastDateT
      val (tf, skf, pf) = loopFwd(prevTime, minSkip)

      // binary search with the interval found
      @tailrec def loopSearch(lo: Long, hi: Long, resTime: Long, resPath: S#Acc): (Long, S#Acc) = {
        if (hi - lo <= minSkip) return (resTime, resPath)

        val mid = (lo + hi) >> 1
        if (LOG_SKIP) println(s"Skip << ${dateFormat.format(new Date(mid))}")
        val pm  = p0.takeUntil(mid)
        if (pm == prevPath) loopSearch(mid, hi, resTime, resPath) else loopSearch(lo, mid, mid, pm)
      }

      val (tb, pb) = loopSearch(tf - skf, tf, tf, pf)
      (tb, pb.info, pb)
    }

    while (t < lastDateT) {
      val (t1, info, p) = masterCursor.step { implicit tx => findNextVersion(t, predPath, skip) }
      val succ = csr.stepFrom(p) { implicit tx =>
        session.collectElements {
          case e: Element.AudioGrapheme[S] => e.entity.value
        } .toSet
      }

      t         = t1
      predPath  = p
      if (succ != pred) {
        val added     = succ -- pred
        val removed   = pred -- succ
        // report()
        val pDate = new Date(info.timeStamp)
        println(s"At ${dateFormat.format(pDate)} added $added, removed $removed")
        pred = succ
      } else {
        val elapsed = System.currentTimeMillis() - reportTime
        if (elapsed >= 30000) report()
        //        if (elapsed >= 60000) {
        //          println("Continue [YES, no] ?")
        //          if (Console.readLine().headOption.map(_.toUpper) == Some('N')) quit()
        //        }

      }
      // t += skip // skip four seconds
    }

    quit()
  }

  def quit(): Unit = {
    println("Closing...")
    session.system.close()
    sys.exit()
  }
}
