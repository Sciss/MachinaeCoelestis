package de.sciss.coelestis

import de.sciss.model.Change
import de.sciss.numbers.Implicits._
import org.jfree.data.statistics.{BoxAndWhiskerItem, DefaultBoxAndWhiskerCategoryDataset}
import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.CategoryPlot
import scalax.chart.Chart
import org.jfree.chart.axis.{NumberAxis, LogarithmicAxis, LogAxis}

object BoxGranularity extends RegionAnalysisLike {
  import RegionAnalysisLike._

  def apply(): Unit = {
    val history = globalHistory(machinaeRegionsFile)

    val states = history.scanLeft(Set.empty[Region])((state, timed) =>
      timed.action match {
        case RegionAdded  (r) => state + r
        case RegionRemoved(r) => state - r
        case RegionMutated(Change(r1, r2), _ ) => state - r1 + r2
        case RegionSplit  (Change(r1, r2), r3) => state - r1 + r2 + r3
      }
    )

    val selected = (1 to 10).map { i => states(i.linlin(0, 10, 0, states.size - 1).toInt) }
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
    val ch = new Chart[CategoryPlot] {
      lazy val peer = chj
      def plot = chj.getPlot.asInstanceOf[CategoryPlot]
    }

    val plot        = ch.plot
    val yAxisLabel  = "region durations [s]"
    val yAxis       = new LogAxis(yAxisLabel) // new LogarithmicAxis(yAxisLabel)
    yAxis.setBase(2)
    yAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
    // yAxis.setAutoRangeNextLogFlag()
    plot.setRangeAxis(yAxis)
    val rnd   = new BoxAndWhiskerRenderer2
    // val r     = plot.getRenderer.asInstanceOf[BoxAndWhiskerRenderer]
    plot.setRenderer(rnd)
    // rnd.setFillBox(false)
    // r.setItemMargin(0.33) - doesn't seem to have any effect...
    rnd.setWhiskerWidth(0.5)
    // rnd.setMeanVisible(false) // makes little sense due to the logarithmic scaling

    ch.printableLook()
    // r.setSeriesPaint(0, Color.lightGray)
    showChart(ch, 400, 400)
  }
}
