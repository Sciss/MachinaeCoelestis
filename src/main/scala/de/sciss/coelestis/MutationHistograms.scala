package de.sciss.coelestis

import de.sciss.file._
import de.sciss.model.Change
import de.sciss.numbers.Implicits._
import scalax.chart.{ChartFactories, Charting}
import Charting._
import org.jfree.chart.plot.ValueMarker
import java.awt.{Font, Color}
import org.jfree.chart.axis.{NumberTickUnit, NumberAxis}
import org.jfree.chart.renderer.xy.{StandardXYBarPainter, XYBarRenderer}

object MutationHistograms extends RegionAnalysisLike {
  import RegionAnalysisLike._

  private def actions(name: String, iteration: Int = -1): Vec[TimedAction] =
    if (name == "Indeterminus" && iteration < 0) {
      (0 to 4).flatMap { iter =>
        val jsonFile = analysisDir / s"${name}_regions$iter.json"
        globalHistory(jsonFile)
      } .sortBy(_.time.version) // sortin not actually necessary for the below plot...
    } else {
      val jsonFile = analysisDir / s"${name}_regions${if (name == "Indeterminus") iteration.toString else ""}.json"
      globalHistory(jsonFile)
    }

  private def resizeData: PartialFunction[TimedAction, Double] = {
    case TimedAction(_, RegionMutated(change, ResizeChange)) =>
      val Change(s1, s2) = change.map(_.time)
      (s2.length - s1.length)/44100.0
  }

  private def moveData: PartialFunction[TimedAction, Double] = {
    case TimedAction(_, RegionMutated(change, MoveChange)) =>
      val Change(s1, s2) = change.map(_.time)
      (s2.start - s1.start)/44100.0
  }

  private def data(actions: Vec[TimedAction], tpe: Mutation): Vec[Double] = {
    val fun = tpe match {
      case ResizeChange => resizeData
      case MoveChange   => moveData
      case _            => throw new IllegalArgumentException(tpe.toString)
    }
    actions.collect(fun)
  }


  def apply(name: String, iteration: Int = -1, tpe: Mutation = ResizeChange): Unit = {
    val history = actions(name, iteration)
    val rel     = data(history, tpe)

    def bin(dur: Double) = ((dur.abs / 0.06).log / 3.log + 1).toInt.clip(0, 10) * dur.signum

    def lim(idx: Int) = if (idx == 0) 0.0 else ((idx.abs - 1) * 3.log).exp * 0.06 * idx.signum

    val binned  = rel.map(bin).counted.toSeq.sortBy(_._1) // .map { case (bin, freq) => lim(bin) -> freq }
    // val ds      = new DefaultCategoryDataset
    // binned.foreach { case (bin, freq) => ds.addValue(freq, "histo", bin) }
    val ds      = binned.toXYSeriesCollection()
    val ch      = ChartFactories.XYBarChart(ds,
      domainAxisLabel = "amount [s]", rangeAxisLabel = "frequency", legend = false)
    val plot    = ch.plot
    plot.addDomainMarker(new ValueMarker(0))

    val xAxis   = plot.getDomainAxis.asInstanceOf[NumberAxis]
    val yAxis   = plot.getRangeAxis

    ch.printableLook()

    xAxis.setTickUnit(new NumberTickUnit(1) {
      override def valueToString(bin: Double) = {
        val sig = if (bin < 0) "\u2212" else if (bin > 0) "+" else ""
        f"""$sig${lim(bin.toInt).abs}%1.2f""" // no unit as it is written in on the axis labl
      }
    })
    xAxis.setVerticalTickLabels(true)
    xAxis.setRange(-9, 9)

    yAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())

    showChart(ch, 500, 375, frameTitle = s"$name : $iteration")
  }
}
