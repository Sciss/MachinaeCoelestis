package de.sciss.coelestis

import de.sciss.model.Change
import de.sciss.numbers.Implicits._
import scalax.chart.{ChartFactories, Charting}
import Charting._
import org.jfree.chart.plot.ValueMarker
import org.jfree.chart.axis.{NumberTickUnit, NumberAxis}

object MutationHistograms extends RegionAnalysisLike {
  import RegionAnalysisLike._

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

  case class Spec(name: String, iteration: Int = -1, split: Split)

  def apply(specs: Seq[Spec], tpe: Mutation = ResizeChange, second: Boolean = false): Unit = {
    // name: String, iteration: Int = -1, tpe: Mutation = ResizeChange
    val rel = specs.toIndexedSeq.flatMap { spec =>
      val h0      = specificHistory(spec.name, spec.iteration)
      val history = spec.split match {
        case SplitNone      => h0
        case SplitHalf      => val (h1, h2) = h0.splitAt(h0.size/2)       ; if (second) h2 else h1
        case SplitAt(time)  => val (h1, h2) = h0.span(_.time.stamp < time); if (second) h2 else h1
      }
      val rel     = data(history, tpe)
      rel
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
    plot.addDomainMarker(new ValueMarker(0))

    val xAxis   = plot.getDomainAxis.asInstanceOf[NumberAxis]
    val yAxis   = plot.getRangeAxis

    ch.printableLook()

    xAxis.setTickUnit(new NumberTickUnit(1) {
      override def valueToString(bin: Double) = {
        // warning: sucky iTextPDF swallows the minus character \u2212, while n-dash works :-E
        val sig = if (bin < 0) "\u2013" /* "\u2212" */ else if (bin > 0) "+" else ""
        f"""$sig${lim(bin.toInt).abs}%1.2f""" // no unit as it is written in on the axis labl
      }
    })
    xAxis.setVerticalTickLabels(true)
    xAxis.setRange(-9, 9)

    yAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())

    val title = specs.map(spec => s"${spec.name} : ${spec.iteration}").mkString(", ")

    showChart(ch, 500, 375, frameTitle = s"$title${if (second) " [second half]" else ""} - $tpe")
  }
}
