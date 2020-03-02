package de.sciss.coelestis

import java.awt.geom.{Ellipse2D, Line2D, Point2D, Rectangle2D}
import java.awt.{AlphaComposite, BasicStroke, Color, Graphics2D, LinearGradientPaint, Paint, Stroke}

import de.sciss.numbers
import org.jfree.chart.axis.{CategoryAxis, ValueAxis}
import org.jfree.chart.plot.{CategoryMarker, CategoryPlot, PlotOrientation}
import org.jfree.chart.renderer.category.{BoxAndWhiskerRenderer, CategoryItemRendererState}
import org.jfree.data.category.CategoryDataset
import org.jfree.data.statistics.BoxAndWhiskerCategoryDataset
import org.jfree.ui.RectangleEdge

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

  override def drawDomainMarker(g2: Graphics2D, plot: CategoryPlot, axis: CategoryAxis,
                                marker: CategoryMarker, dataArea: Rectangle2D): Unit = marker match {
    case _: LeftCategoryMarker => drawLeftMarker(g2, plot, axis, marker, dataArea)
    case _ => super.drawDomainMarker(g2, plot, axis, marker, dataArea)
  }

  private def drawLeftMarker(g2: Graphics2D, plot: CategoryPlot, axis: CategoryAxis,
                             marker: CategoryMarker, dataArea: Rectangle2D): Unit = {
    val cat     = marker.getKey
    val dataset = plot.getDataset(plot.getIndexOf(this))
    val catIdx  = dataset.getColumnIndex(cat)
    if (catIdx < 0) return

    val savedComposite = g2.getComposite
    g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, marker.getAlpha))

    axis.getCategoryMargin
    val numCat      = dataset.getColumnCount
    val domainEdge  = plot.getDomainAxisEdge
    val left        = axis.getCategoryStart   (catIdx, numCat, dataArea, domainEdge)
    val gap         = calculateCategoryGapSize(axis  , numCat, dataArea, domainEdge)
    val v           = left - gap/2 // axis.getCategoryMiddle(columnIndex, dataset.getColumnCount, dataArea, domainEdge)
    val line        = if (plot.getOrientation == PlotOrientation.HORIZONTAL)
      new Line2D.Double(dataArea.getMinX, v, dataArea.getMaxX, v)
    else
      new Line2D.Double(v, dataArea.getMinY, v, dataArea.getMaxY)

    g2.setPaint (marker.getPaint )
    g2.setStroke(marker.getStroke)
    g2.draw(line)

    g2.setComposite(savedComposite)
  }

  private def calculateCategoryGapSize(axis: CategoryAxis, categoryCount: Int, area: Rectangle2D,
                                       edge: RectangleEdge): Double = {
    if (categoryCount == 0) return 0.0

    val available = if ((edge == RectangleEdge.TOP) || (edge == RectangleEdge.BOTTOM))
      area.getWidth
    else if ((edge == RectangleEdge.LEFT) || (edge == RectangleEdge.RIGHT))
      area.getHeight
    else
      0.0

    available * axis.getCategoryMargin / (categoryCount - 1)
  }

  var meanWidth     = 0.2
  var meanFilled    = false
  var whiskerStroke: Option[Stroke] = Some(new BasicStroke(1.0f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10.0f,
    Array[Float](3f, 3f), 0.0f))
  var boxFillPaint: Paint = new LinearGradientPaint(0f, 0f, 0.5f, 0f, Array[Float](0f, 1f),
    Array[Color](new Color(0xC0, 0xC0, 0xC0, 0xFF), new Color(0xC0, 0xC0, 0xC0, 0x00)))
}