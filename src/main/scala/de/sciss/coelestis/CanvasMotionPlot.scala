package de.sciss.coelestis

import java.awt.geom.{Area, GeneralPath, Line2D}
import java.awt.{BasicStroke, Color}

import de.sciss.file._
import de.sciss.pdflitz

import scala.swing.Swing._

object CanvasMotionPlot extends RegionAnalysisLike {

  val useVersions = false
  val connect     = true
  // val yscale      = 1.5

  case class Line(id: Int, y: Int, x0: Int, x1: Int)

  def apply(dataBase: String, yscale: Double = 1.5, split: Int = -1): Unit = {
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
      if (split > 0)  {
        require(!useVersions)
        val sIdx  = history.indexWhere(_.time.stamp >= indetSplits(split))
        val sy    = sIdx * yscale
        val ln    = new Line2D.Double(0, sy, w, sy)
        g.setColor(new Color(0, 0, 0, 0x7F))
        val strk2 = new BasicStroke(1f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 4f, Array[Float](2f, 2f), 0f)
        g.fill(strk2.createStrokedShape(ln))
      }

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
