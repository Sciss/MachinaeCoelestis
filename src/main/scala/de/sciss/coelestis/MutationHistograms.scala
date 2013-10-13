package de.sciss.coelestis

import de.sciss.file._
import de.sciss.model.Change
import de.sciss.numbers.Implicits._
import scalax.chart.{ChartFactories, Charting}
import Charting._

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

    def bin(dur: Double) = ((dur.abs / 0.02).log / 3.log + 1).toInt.clip(0, 10) * dur.signum

    val binned  = rel.map(bin).counted.toSeq.sortBy(_._1)
    val ds      = binned.toCategoryDataset
    val ch      = ChartFactories.BarChart(ds)

    showChart(ch, 600, 400)
  }
}
