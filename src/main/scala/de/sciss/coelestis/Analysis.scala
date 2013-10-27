package de.sciss.coelestis

object Analysis extends App {
  import RegionAnalysisLike._
  import RegionAnalysis._

  // AudioFileAnalysis()
  // RegionAnalysis()

  //    RegionAnalysis(files = Seq(
  //      indetFile(0) -> SplitNone
  //    ), percent = false)

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

  // CanvasMotionPlot("Indeterminus_regions1", split = 1)
  // CanvasMotionPlot("Indeterminus_regions2", split = 2)
  CanvasMotionPlot("Indeterminus_regions3", split = 3)

  // CanvasMotionPlot("machinae_regions")

  import MutationHistograms.Spec

  //  MutationHistograms(Seq(
  //    Spec("machinae", iteration = -1, split = SplitNone)
  //  ), tpe = ResizeChange, second = false)

  //  MutationHistograms(Seq(
  //    Spec("Indeterminus", iteration = -1, split = SplitNone)
  //  ), tpe = ResizeChange, second = false)

  //  MutationHistograms(Seq(
  //    Spec("Indeterminus", iteration = 0, split = SplitNone)
  //  ), tpe = ResizeChange, second = false)

  //  MutationHistograms(Seq(
  //    Spec("Indeterminus", iteration = 1, split = SplitAt(indetSplits(1))),
  //    Spec("Indeterminus", iteration = 2, split = SplitAt(indetSplits(2))),
  //    Spec("Indeterminus", iteration = 3, split = SplitAt(indetSplits(3))),
  //    Spec("Indeterminus", iteration = 4, split = SplitNone)
  //  ), tpe = ResizeChange, second = false)

  //  MutationHistograms(Seq(
  //    Spec("Indeterminus", iteration = 1, split = SplitAt(indetSplits(1))),
  //    Spec("Indeterminus", iteration = 2, split = SplitAt(indetSplits(2))),
  //    Spec("Indeterminus", iteration = 3, split = SplitAt(indetSplits(3)))
  //  ), tpe = ResizeChange, second = true)

  //  MutationHistograms(Seq(
  //    Spec("machinae", iteration = -1, split = SplitNone)
  //  ), tpe = MoveChange, second = false)

  //  MutationHistograms(Seq(
  //    Spec("Indeterminus", iteration = -1, split = SplitNone)
  //  ), tpe = MoveChange, second = false)

  //  MutationHistograms(Seq(
  //    Spec("Indeterminus", iteration = 0, split = SplitNone)
  //  ), tpe = MoveChange, second = false)

  //  MutationHistograms(Seq(
  //    Spec("Indeterminus", iteration = 1, split = SplitAt(indetSplits(1))),
  //    Spec("Indeterminus", iteration = 2, split = SplitAt(indetSplits(2))),
  //    Spec("Indeterminus", iteration = 3, split = SplitAt(indetSplits(3))),
  //    Spec("Indeterminus", iteration = 4, split = SplitNone)
  //  ), tpe = MoveChange, second = false)

  //  MutationHistograms(Seq(
  //    Spec("Indeterminus", iteration = 1, split = SplitAt(indetSplits(1))),
  //    Spec("Indeterminus", iteration = 2, split = SplitAt(indetSplits(2))),
  //    Spec("Indeterminus", iteration = 3, split = SplitAt(indetSplits(3)))
  //  ), tpe = MoveChange, second = true)

  // PunchCard()

  // BoxGranularity("Indeterminus", iteration = 1)
  // BoxGranularity("Indeterminus", iteration = 2)
  // BoxGranularity("Indeterminus", iteration = 3)
  // BoxGranularity("Indeterminus", iteration = -1)

  // BoxGranularity("machinae")
}