package de.sciss.coelestis

import de.sciss.pdflitz
import de.sciss.file._
import scala.swing.Swing
import Swing._
import java.awt.Color

object CanvasMotionPlot extends RegionAnalysisLike {
  val useVersions = false

  def apply(): Unit = {
    val pdfFile = desktop / "machinae_canvasmotion.pdf"
    if (pdfFile.isFile) {
      println(s"Already generated '$pdfFile'")
      return
    }

    val history = globalHistory()
    val vFirst  = history.head.time.version
    val vLast   = history.last.time.version
    val span    = history.view.map(_.action.actionSpan).reduce(_ union _)
    println(s"vFirst = $vFirst, vLast = $vLast, span = $span")

    val w       = (span.length / 44100).toInt
    val h       = if (useVersions) vLast - vFirst + 1 else history.size

    val view = pdflitz.Generate.QuickDraw((w, h)) { g =>
      g.setColor(Color.black)
      history.zipWithIndex.foreach { case (timed, idx) =>
        val y   = if (useVersions) timed.time.version - vFirst else idx
        val s   = timed.action.actionSpan
        val x0  = ((s.start - span.start)/44100).toInt
        val x1  = ((s.stop  - span.start)/44100).toInt
        g.drawLine(x0, y, x1, y)
      }
    }

    pdflitz.Generate(pdfFile, view)
  }
}
