package de.sciss.coelestis

object Analysis extends App {
  import RegionAnalysisLike._

  // AudioFileAnalysis()
  // RegionAnalysis()
  // CanvasMotionPlot("Indeterminus_regions4")

  // MutationHistograms("Indeterminus", iteration = 4, tpe = MoveChange)
  MutationHistograms("machinae", iteration = 0, tpe = MoveChange)
}