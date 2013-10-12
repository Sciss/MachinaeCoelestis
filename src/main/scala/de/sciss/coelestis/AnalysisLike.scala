package de.sciss.coelestis

import de.sciss.lucre.confluent.{VersionInfo, Cursor}
import de.sciss.lucre.synth.expr.Strings
import java.util.Date
import scala.annotation.tailrec

trait AnalysisLike {
  lazy val masterCursor: Cursor[S, D] = session.cursors.cursor

  var LOG_SKIP  = false

  def analysisCursor: Cursor[S, D] = {
    requireNoTxn()
    masterCursor.step { implicit tx =>
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

  def quit(): Unit = {
    println("Closing...")
    if (sessionOpen) session.system.close()
    sys.exit()
  }
}
