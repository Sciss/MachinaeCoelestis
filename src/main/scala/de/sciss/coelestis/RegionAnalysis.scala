package de.sciss.coelestis

import scalax.chart
import java.awt.{Font, Color}
import de.sciss.intensitypalette.IntensityPalette
import de.sciss.numbers
import de.sciss.file.File

object RegionAnalysis extends RegionAnalysisLike {
  import RegionAnalysisLike._

  def reportFileChanges = true

  def apply(jsonFile: File = machinaeRegionsFile, percent: Boolean = true): Unit = {
    generateJSON(jsonFile)(plotGlobal(jsonFile, percent = percent))
  }

  val PieTypes = Seq(RegionAdded, RegionRemoved, RegionSplit, MoveChange, ResizeChange, GainChange, FadeChange, MuteChange)

  def plotGlobal(jsonFile: File, percent: Boolean): Unit = {
    val history = globalHistory(jsonFile)

    if (reportFileChanges) {
      history.foreach {
        case action @ TimedAction(_, RegionMutated(_, FileChange)) => println(action)
        case _ =>
      }
    }

    import chart._
    import Charting._

    def plot1(data: Vec[TimedAction], title: String): Unit = {
      val sum0 = data.map(_.action match {
        case RegionMutated(_, m) => m
        case a => a.tpe
      }).counted

      //    println(s"history ${history.size}, counted ${sum0.size}")
      //    println(sum0.map { case (action, count) => action.name -> count })

      val sum1 = sum0.toIndexedSeq.sortBy(x => PieTypes.indexOf(x._1)).map { case (action, count) => action.name -> count }
      val sum  = if (percent) {
        val n = sum1.map(_._2).sum
        println(s"Total number of actions: $n")
        val f = 100.0/n
        sum1.map { case (action, count) => action -> (count * f + 0.5).toInt }
      } else {
        sum1
      }

      val sumM = sum.toMap

      val ds: PieDataset = sum.toPieDataset
      val ch    = ChartFactories.PieChart(ds, legend = false)
      val plot  = ch.plot
      val tpeSz = PieTypes.size
      PieTypes.zipWithIndex.foreach { case (tpe, idx) =>
        import numbers.Implicits._
        // val colr = if ((idx % 2) == 0) Color.darkGray else Color.lightGray
        val in = idx.linlin(-0.5, tpeSz - 0.5, 1.0, 0.0) + (if ((idx % 2) == 1) 0.5 else 0.0)
        val colr = new Color(IntensityPalette.apply((in % 1.0).toFloat))
        plot.setSectionPaint(tpe.name, colr)
      }
      // plot.setSectionPaint(RegionRemoved.name, Color.white)
      plot.setBackgroundPaint(Color.white)
      plot.setLabelBackgroundPaint(null)
      plot.setLabelOutlinePaint(null)
      plot.setLabelShadowPaint(null)
      plot.setLabelFont(new Font("Helvetica", Font.PLAIN, 12))
      plot.setOutlineVisible(false)

      val lbGen: (org.jfree.data.general.PieDataset,Comparable[_]) => String = if (percent) {
        (_, key) => s"$key ${sumM(key.toString)}%"
      }
      else { (_, key) =>
        s"${sumM(key.toString)}\u00D7 $key"
      }
      ch.labelGenerator = Some(lbGen)

      showChart(ch, w = 600, h = 400, frameTitle = title)
    }

    val (h1, h2) = history.splitAt(history.size/2)
    plot1(h1, "First Half")
    plot1(h2, "Second Half")
  }
}