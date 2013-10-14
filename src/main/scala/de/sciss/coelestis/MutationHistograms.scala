package de.sciss.coelestis

import de.sciss.file._
import de.sciss.model.Change
import de.sciss.numbers.Implicits._
import scalax.chart.{ChartFactories, Charting}
import Charting._
import org.jfree.chart.plot.ValueMarker
import java.awt.Color
import org.jfree.chart.{ChartFactory, StandardChartTheme}
import org.jfree.chart.axis.{NumberTickUnit, NumberAxis}
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.chart.renderer.category.{BarRenderer, StandardBarPainter}
import java.text.{FieldPosition, ParsePosition, NumberFormat}
import org.jfree.chart.renderer.xy.{StandardXYBarPainter, XYBarRenderer}

object MutationHistograms extends RegionAnalysisLike {
  import RegionAnalysisLike._

  def jsonFile: File = machinaeRegionsFile

  def apply(): Unit = {
    val history = globalHistory()
    val resize  = history.collect {
      case TimedAction(_, RegionMutated(change, ResizeChange)) => change.map(_.time)
    }
    val rel = resize.map { case Change(s1, s2) =>
      // s2.length.toDouble / s1.length
      (s2.length - s1.length)/44100.0
    }

    def bin(dur: Double) = ((dur.abs / 0.06).log / 3.log + 1).toInt.clip(0, 10) * dur.signum

    def lim(idx: Int) = if (idx == 0) 0.0 else ((idx.abs - 1) * 3.log).exp * 0.06 * idx.signum

    val binned  = rel.map(bin).counted.toSeq.sortBy(_._1) // .map { case (bin, freq) => lim(bin) -> freq }
    // val ds      = new DefaultCategoryDataset
    // binned.foreach { case (bin, freq) => ds.addValue(freq, "histo", bin) }
    val ds      = binned.toXYSeriesCollection()
    val ch      = ChartFactories.XYBarChart(ds,
      domainAxisLabel = "amount [s]", rangeAxisLabel = "frequency", legend = false)
    val plot    = ch.plot
    plot.getRenderer.asInstanceOf[XYBarRenderer].setBarPainter(new StandardXYBarPainter())
    // plot.addDomainMarker(new ValueMarker(0))
    plot.setBackgroundPaint(Color.white)
    plot.setDomainGridlinePaint(Color.lightGray)
    plot.setRangeGridlinePaint (Color.lightGray)
    val r       = plot.getRenderer // .asInstanceOf[]
    r.setSeriesPaint(0, Color.darkGray)
    val xAxis   = plot.getDomainAxis.asInstanceOf[NumberAxis]
    xAxis.setTickUnit(new NumberTickUnit(1) {
      override def valueToString(bin: Double) = f"${if (bin > 0) "+" else ""}${lim(bin.toInt)}%1.2f'"
    })
    //    xAxis.setNumberFormatOverride(new NumberFormat {
    //      def format(value: Double, sb: StringBuffer, pos: FieldPosition) =
    //        sb.append(f"${lim(value.toInt)}%1.2f")
    //
    //      def format(value: Long, sb: StringBuffer, pos: FieldPosition) = throw new NotImplementedError()
    //      def parse(source: String, parsePosition: ParsePosition) = throw new NotImplementedError()
    //    })

    // xAxis.setLabelAngle(45.0.toRadians)
    xAxis.setVerticalTickLabels(true)

    // ChartFactory.setChartTheme(StandardChartTheme.createLegacyTheme())

    showChart(ch, 600, 400)
  }
}
