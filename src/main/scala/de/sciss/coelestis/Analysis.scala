package de.sciss.coelestis

object Analysis extends App {
  import RegionAnalysisLike._

  // AudioFileAnalysis()
  // RegionAnalysis()
  RegionAnalysis(jsonFile = indetFile(4))
  // CanvasMotionPlot("Indeterminus_regions4")
  // CanvasMotionPlot("machinae_regions")

  // MutationHistograms("Indeterminus", iteration = 4, tpe = MoveChange)
  // MutationHistograms("machinae", iteration = 0, tpe = MoveChange)
  // PunchCard()
  // BoxGranularity("Indeterminus", iteration = -1)
  // BoxGranularity("machinae")
}