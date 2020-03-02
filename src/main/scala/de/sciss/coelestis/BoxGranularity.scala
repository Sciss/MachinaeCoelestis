package de.sciss.coelestis

import java.awt.{BasicStroke, Color}

import de.sciss.model.Change
import de.sciss.numbers.Implicits._
import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.chart.axis.{LogAxis, NumberAxis}
import org.jfree.chart.plot.CategoryPlot
import org.jfree.data.statistics.{BoxAndWhiskerItem, DefaultBoxAndWhiskerCategoryDataset}
import org.jfree.ui.Layer
import scalax.chart.Chart

object BoxGranularity extends RegionAnalysisLike {
  import RegionAnalysisLike._

  def apply(name: String, iteration: Int = 0): Unit = {
    val history   = specificHistory(name, iteration)
    val splitIdx  = if (name == "Indeterminus" && (iteration == -1 || (iteration >= 1 && iteration <= 3))) {
      val t = indetSplits(iteration.max(1)) // all iterations = use first split
      history.indexWhere(_.time.stamp >= t)
    } else -1

    val states = history.scanLeft(Set.empty[Region])((state, timed) =>
      timed.action match {
        case RegionAdded  (r) => state + r
        case RegionRemoved(r) => state - r
        case RegionMutated(Change(r1, r2), _ ) => state - r1 + r2
        case RegionSplit  (Change(r1, r2), r3) => state - r1 + r2 + r3
      }
    )

    def binIdx(bin: Int) = bin.linlin(0, 10, 0, states.size - 1).toInt

    val splitBin = (1 to 10).find(binIdx(_) >= splitIdx).getOrElse(-1)

    if (splitIdx != -1) println(s"Recursion bin is $splitBin")

    val selected = (1 to 10).map { i => states(binIdx(i)) }
    // selected.foreach(x => println(x.size))

    val ds = new DefaultBoxAndWhiskerCategoryDataset()
    selected.zipWithIndex.foreach { case (set, idx) =>
      val durs    = set.toIndexedSeq.map(_.time.length / 44100.0).sorted
      val n       = durs.size
      // val (mean, _ /* vari */) = durs.meanVariance // durs.sum / n
      val median  = durs.percentile(50) // if (n % 2 == 1) durs(n/2) else (durs(n/2) + durs(n/2+1))/2
      val q1      = durs.percentile(25) // durs((n  /4.0 + 0.5).toInt)
      val q3      = durs.percentile(75) // durs((n*3/4.0 + 0.5).toInt)
      // cf. https://en.wikipedia.org/wiki/Boxplot -> types of box plots
      // val min     = durs.min
      // val max     = durs.max
      // val stddev  = math.sqrt(vari / (n-1))
      // val min     = mean - stddev
      // val max     = mean + stddev
      // val min     = durs.percentile( 9)
      // val max     = durs.percentile(91)
      val min     = durs.percentile( 2)
      val max     = durs.percentile(98)

      val mean    = math.pow(durs.product, 1.0/n)
      val item = new BoxAndWhiskerItem(mean, median, q1, q3, min, max, 0.0, 0.0, new java.util.ArrayList[Any])
      ds.add(item, "row", idx)
    }

    val chj = ChartFactory.createBoxAndWhiskerChart(/* title = */ null, null /* "categ" */, "value", ds, /* legend = */ false)
    val ch: Chart[CategoryPlot] = new Chart[CategoryPlot] {
      lazy val peer: JFreeChart = chj
      def plot: CategoryPlot = chj.getPlot.asInstanceOf[CategoryPlot]
    }

    val plot        = ch.plot
    val yAxisLabel  = "region durations [s]"
    val yAxis       = new LogAxis(yAxisLabel) // new LogarithmicAxis(yAxisLabel)
    yAxis.setBase(2)
    yAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
    // yAxis.setAutoRangeNextLogFlag()
    plot.setRangeAxis(yAxis)
    // println(s"yAxis low ${yAxis.getLowerBound} high ${yAxis.getUpperBound}")
    yAxis.setLowerBound(0.5)
    yAxis.setUpperBound(450)
    val rnd   = new BoxAndWhiskerRenderer2
    // val r     = plot.getRenderer.asInstanceOf[BoxAndWhiskerRenderer]
    plot.setRenderer(rnd)
    // rnd.setFillBox(false)
    // r.setItemMargin(0.33) - doesn't seem to have any effect...
    rnd.setWhiskerWidth(0.5)
    // rnd.setMeanVisible(false) // makes little sense due to the logarithmic scaling
    val xAxis = plot.getDomainAxis
    xAxis.setVisible(false)

    println(xAxis)

    if (splitBin > 0) {
      // plot.addDomainMarker(new ValueMarker(splitBin))
      val mark = new LeftCategoryMarker(splitBin - 1, Color.gray,
        new BasicStroke(1f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 4f, Array[Float](2f, 2f), 0f))
      mark.setDrawAsLine(true)
      plot.addDomainMarker(mark, Layer.BACKGROUND)
    }

    ch.printableLook()
    // r.setSeriesPaint(0, Color.lightGray)
    showChart(ch, 400, 400, frameTitle = s"$name : $iteration")
  }
}
