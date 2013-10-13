package de.sciss.coelestis

import de.sciss.pdflitz
import de.sciss.file._
import scala.swing.Swing
import Swing._
import java.awt.Color
import java.awt.geom.GeneralPath

object CanvasMotionPlot extends RegionAnalysisLike {
  val useVersions = false
  val connect     = true

  case class Line(id: Int, y: Int, x0: Int, x1: Int)

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
      val lines = history.zipWithIndex.map { case (timed, idx) =>
        val y   = if (useVersions) timed.time.version - vFirst else idx
        val s   = timed.action.actionSpan
        val x0  = ((s.start - span.start)/44100).toInt
        val x1  = ((s.stop  - span.start)/44100).toInt
        Line(id = timed.action.inputRegion.id, y = y, x0 = x0, x1 = x1)
      }

      if (connect) {
        val obj = lines.groupWith(_.id == _.id)
        obj.foreach { case head +: tail0 =>
          val tail = if (tail0.isEmpty) Vec(head.copy(y = head.y + 1)) else tail0
          val gp = new GeneralPath()
          gp.moveTo(head.x0, head.y)
          tail.foreach { line =>
            import line._
            gp.lineTo(x0, y)
          }
          tail.reverse.foreach { line =>
            import line._
            gp.lineTo(x1, y)
          }
          gp.lineTo(head.x1, head.y)
          gp.closePath()
          g.fill(gp)
        }

      } else {
        lines.foreach { line =>
          import line._
          g.drawLine(x0, y, x1, y)
        }
      }
    }

    pdflitz.Generate(pdfFile, view)
  }
}
