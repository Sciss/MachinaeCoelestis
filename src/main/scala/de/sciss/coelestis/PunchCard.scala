package de.sciss.coelestis

import java.util.{TimeZone, Calendar}
import scalax.chart.Chart
import org.jfree.data.xy.{MatrixSeriesCollection, MatrixSeries}
import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.{XYPlot, PlotOrientation}
import org.jfree.chart.axis.{NumberTickUnit, NumberAxis}
import de.sciss.numbers.Implicits._
import java.awt.Color
import de.sciss.file._

object PunchCard extends RegionAnalysisLike {
  def apply(name: String = "Indeterminus"): Unit = {
    val history   = if (name == "Indeterminus") {
      (0 to 4).flatMap { iter =>
        val jsonFile = analysisDir / s"${name}_regions$iter.json"
        globalHistory(jsonFile)
      } .sortBy(_.time.version) // sortin not actually necessary for the below plot...
    } else {
      globalHistory(machinaeRegionsFile)
    }
    val times     = history.map { timed =>
      val date  = timed.time.date
      val cal   = Calendar.getInstance(TimeZone.getTimeZone("Europe/Berlin"))
      cal.setTime(date)
      val day   = cal.get(Calendar.DAY_OF_WEEK) // 1 = Sunday, 7 = Saturday
      val hour  = cal.get(Calendar.HOUR_OF_DAY) // 24 based
      (day, hour)
    }
    val data  = times.counted
    val max   = data.values.max
    // `sqrt` for better visibilty, `* 0.5` because the circles are too large
    val norm  = Array.tabulate(7, 24)((d, h) => (data(d, h).toDouble / max).sqrt * 0.5)

    val ms = new MatrixSeries("punch", 7, 24) {
      data = norm
    }
    val mc  = new MatrixSeriesCollection(ms)
    val chj = ChartFactory.createBubbleChart(/* title = */ null, /* xAxisLabel = */ "hour",
      /* yAxisLabel = */ null, /* dataset = */ mc, /* orientation = */ PlotOrientation.VERTICAL,
      /* legend = */ false, /* tooltips = */ false, /* urls = */ false)

    val ch = new Chart[XYPlot] {
      lazy val peer = chj
      def plot = chj.getPlot.asInstanceOf[XYPlot]
    }

    val plot = ch.plot
    plot.getRenderer.setSeriesOutlinePaint(0, new Color(0, 0, 0, 0)) // null
    // plot.getRenderer.setSeriesOutlineStroke(0, null)
    // plot.getRenderer.setSeriesPaint       (0, Color.darkGray)

    val xAxis = plot.getDomainAxis
    val yAxis = plot.getRangeAxis
    xAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
    yAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
    xAxis.setRange(-0.5, 23.5)
    yAxis.setRange( 0.5, 7.5)

    yAxis.asInstanceOf[NumberAxis].setTickUnit(new NumberTickUnit(1) {
      private val days = Array("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
      override def valueToString(day: Double) = days(day.toInt - 1)
    })

    ch.printableLook()
    showChart(ch, 600, 300)
  }
}
