package de.sciss.coelestis

import de.sciss.pdflitz
import de.sciss.file._
import scala.swing.Swing
import Swing._
import java.awt.{BasicStroke, Color}
import java.awt.geom.{Area, GeneralPath}

object CanvasMotionPlot extends RegionAnalysisLike {
  val useVersions = false
  val connect     = true
  // val yscale      = 1.5

  case class Line(id: Int, y: Int, x0: Int, x1: Int)

  def apply(dataBase: String, yscale: Double = 1.5): Unit = {
    val jsonFile  = analysisDir / s"$dataBase.json"

    val pdfFile = desktop / s"${jsonFile.base}_canvasmotion.pdf"
    if (pdfFile.isFile) {
      println(s"Already generated '$pdfFile'")
      return
    }

    val history = globalHistory(jsonFile)
    val vFirst  = history.head.time.version
    val vLast   = history.last.time.version
    val span    = history.view.map(_.action.actionSpan).reduce(_ union _)
    println(s"vFirst = $vFirst, vLast = $vLast, span = $span")

    val w       = (span.length / 44100).toInt
    val h       = math.ceil((if (useVersions) vLast - vFirst + 1 else history.size) * yscale).toInt

    val view = pdflitz.Generate.QuickDraw((w, h)) { g =>
      g.setColor(Color.black)
      g.scale(1.0, yscale)
      val lines = history.zipWithIndex.map { case (timed, idx) =>
        val y   = if (useVersions) timed.time.version - vFirst else idx
        val s   = timed.action.actionSpan
        val x0  = ((s.start - span.start)/44100).toInt
        val x1  = ((s.stop  - span.start)/44100).toInt
        Line(id = timed.action.inputRegion.id, y = y, x0 = x0, x1 = x1)
      }

      if (connect) {
        val strk = new BasicStroke(1f)
        val obj = lines.groupWith(_.id == _.id)
        obj.foreach { case head +: tail =>
          // val tail = if (tail0.isEmpty) Vec(head.copy(y = head.y + 1)) else tail0
          val gp = new GeneralPath()
          gp.moveTo(head.x0 + 0.5f, head.y + 0.5f)
          tail.foreach { line =>
            import line._
            gp.lineTo(x0 + 0.5f, y + 0.5f)
          }
          tail.reverse.foreach { line =>
            import line._
            gp.lineTo(x1 - 0.5f, y + 0.5f)
          }
          gp.lineTo(head.x1 - 0.5f, head.y + 0.5f)
          gp.closePath()
          val outline = strk.createStrokedShape(gp)
          val area    = new Area(gp)
          area.add(new Area(outline))
          g.fill(area)
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
