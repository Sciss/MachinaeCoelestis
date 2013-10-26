package de.sciss.coelestis

object Analysis extends App {
  import RegionAnalysisLike._
  import RegionAnalysis._

  // AudioFileAnalysis()
  // RegionAnalysis()

    RegionAnalysis(files = Seq(
      indetFile(0) -> SplitNone
    ), percent = false)

  //  RegionAnalysis(files = Seq(
  //    // indetFile(0) -> SplitNone,
  //    indetFile(1) -> SplitAt(indetSplits(1)),
  //    indetFile(2) -> SplitAt(indetSplits(2)),
  //    indetFile(3) -> SplitAt(indetSplits(3)),
  //    indetFile(4) -> SplitNone
  //  ), percent = false)

  //  RegionAnalysis(files = Seq(
  //    indetFile(0) -> SplitNone,
  //    indetFile(1) -> SplitAt(indetSplits(1))
  //  ), percent = false)

  //  RegionAnalysis(files = Seq(
  //    indetFile(2) -> SplitAt(indetSplits(2)),
  //    indetFile(3) -> SplitAt(indetSplits(3)),
  //    indetFile(4) -> SplitNone
  //  ), percent = false)


  // CanvasMotionPlot("Indeterminus_regions4")
  // CanvasMotionPlot("machinae_regions")

  // MutationHistograms("Indeterminus", iteration = 4, tpe = MoveChange)
  // MutationHistograms("machinae", iteration = 0, tpe = MoveChange)
  // PunchCard()
  // BoxGranularity("Indeterminus", iteration = -1)
  // BoxGranularity("machinae")
}