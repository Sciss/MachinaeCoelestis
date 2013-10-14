package de.sciss.coelestis

import org.jfree.chart.renderer.category.{CategoryItemRendererState, BoxAndWhiskerRenderer}
import java.awt.{Color, Paint, LinearGradientPaint, Stroke, BasicStroke, Graphics2D}
import java.awt.geom.{Point2D, Ellipse2D, Line2D, Rectangle2D}
import org.jfree.chart.plot.CategoryPlot
import org.jfree.chart.axis.{ValueAxis, CategoryAxis}
import org.jfree.data.category.CategoryDataset
import org.jfree.data.statistics.BoxAndWhiskerCategoryDataset
import de.sciss.numbers

class BoxAndWhiskerRenderer2 extends BoxAndWhiskerRenderer {
  override def drawVerticalItem(g: Graphics2D, state: CategoryItemRendererState, dataArea: Rectangle2D,
                                plot: CategoryPlot, xAxis: CategoryAxis, yAxis: ValueAxis,
                                data: CategoryDataset, row: Int, column: Int): Unit = {

    val bData   = data.asInstanceOf[BoxAndWhiskerCategoryDataset]

    val xEnd    = xAxis.getCategoryEnd  (column, getColumnCount, dataArea, plot.getDomainAxisEdge)
    val xStart  = xAxis.getCategoryStart(column, getColumnCount, dataArea, plot.getDomainAxisEdge)
    val width   = xEnd - xStart

    val seriesCount   = getRowCount
    val categoryCount = getColumnCount
    val bw            = state.getBarWidth

    val xx = if (seriesCount > 1) {
      val seriesGap = dataArea.getWidth * getItemMargin / (categoryCount * (seriesCount - 1))
      val usedWidth = bw * seriesCount + seriesGap * (seriesCount - 1)
      val offset    = (width - usedWidth) / 2
      xStart + offset + row * (bw + seriesGap)
    } else {
      val offset    = (width - bw) / 2
      xStart + offset
    }

    val itemPaint = getItemPaint(row, column)
    g.setPaint(itemPaint)
    val s = getItemStroke(row, column)
    g.setStroke(s)

    val location = plot.getRangeAxisEdge

    val yQ1   = bData.getQ1Value        (row, column)
    val yQ3   = bData.getQ3Value        (row, column)
    val yMax  = bData.getMaxRegularValue(row, column)
    val yMin  = bData.getMinRegularValue(row, column)

    if (yQ1 != null && yQ3 != null && yMax != null && yMin != null) {
      val yyQ1  = yAxis.valueToJava2D(yQ1 .doubleValue(), dataArea, location)
      val yyQ3  = yAxis.valueToJava2D(yQ3 .doubleValue(), dataArea, location)
      val yyMax = yAxis.valueToJava2D(yMax.doubleValue(), dataArea, location)
      val yyMin = yAxis.valueToJava2D(yMin.doubleValue(), dataArea, location)
      val xxmid = xx + bw / 2.0
      val halfW = (bw / 2.0) * getWhiskerWidth

      val box = new Rectangle2D.Double(xx, math.min(yyQ1, yyQ3), bw, math.abs(yyQ1 - yyQ3))
      if (getFillBox) {
        // val atOrig = g.getTransform
        // g.translate(box.getX, box.getY)
        // g.scale(box.getWidth, box.getHeight)
        val pnt = boxFillPaint match {
          case lp: LinearGradientPaint =>
            import numbers.Implicits._
            def trans(in: Point2D) =
              new Point2D.Double(in.getX.linlin(0, 1, box.getX, box.getX + box.getWidth),
                                 in.getY.linlin(0, 1, box.getY, box.getX + box.getHeight))
            new LinearGradientPaint(trans(lp.getStartPoint), trans(lp.getEndPoint), lp.getFractions, lp.getColors)
          case other => other
        }
        g.setPaint(pnt)
        g.fill(box) // new Rectangle2D.Double(0, 0, 1.0, 1.0))
        g.setPaint(itemPaint)
        // g.setTransform(atOrig)
      }

      val outlinePaint = getItemOutlinePaint(row, column)
      if (getUseOutlinePaintForWhiskers) g.setPaint(outlinePaint)

      // val strkOrig = g.getStroke
      g.draw(new Line2D.Double(xxmid - halfW, yyMax, xxmid + halfW, yyMax))
      g.draw(new Line2D.Double(xxmid - halfW, yyMin, xxmid + halfW, yyMin))

      whiskerStroke.foreach(g.setStroke)

      g.draw(new Line2D.Double(xxmid, yyMax, xxmid, yyQ3))
      g.draw(new Line2D.Double(xxmid, yyMin, xxmid, yyQ1))

      g.setStroke(getItemOutlineStroke(row, column))
      g.setPaint(outlinePaint)
      g.draw(box)
    }

    g.setPaint(getArtifactPaint)

    // draw mean - SPECIAL AIMS REQUIREMENT...
    if (isMeanVisible) {
      val yMean = bData.getMeanValue(row, column)
      if (yMean != null) {
        val yyAverage = yAxis.valueToJava2D(yMean.doubleValue(), dataArea, location)
        val aRadius = bw * meanWidth * 0.5
        // here we check that the average marker will in fact be
        // visible before drawing it...
        if ((yyAverage > (dataArea.getMinY - aRadius)) &&
            (yyAverage < (dataArea.getMaxY + aRadius))) {
          val avgEllipse = new Ellipse2D.Double(xx + bw/2 - aRadius, yyAverage - aRadius, aRadius * 2, aRadius * 2)
          if (meanFilled) g.fill(avgEllipse)
          g.draw(avgEllipse)
        }
      }
    }

    // draw median...
    if (isMedianVisible) {
      val yMedian = bData.getMedianValue(row, column)
      if (yMedian != null) {
        val yyMedian = yAxis.valueToJava2D(yMedian.doubleValue(), dataArea, location)
        g.draw(new Line2D.Double(xx, yyMedian, xx + state.getBarWidth, yyMedian))
      }
    }
  }

  var meanWidth     = 0.2
  var meanFilled    = false
  var whiskerStroke = Option[Stroke](new BasicStroke(1.0f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10.0f,
    Array[Float](3f, 3f), 0.0f))
  var boxFillPaint: Paint = new LinearGradientPaint(0f, 0f, 0.5f, 0f, Array[Float](0f, 1f),
    Array[Color](new Color(0xC0, 0xC0, 0xC0, 0xFF), new Color(0xC0, 0xC0, 0xC0, 0x00)))
}