package de.sciss.coelestis

import org.jfree.chart.plot.CategoryMarker
import java.awt.{Color, BasicStroke, Paint, Stroke}

class LeftCategoryMarker(key: Comparable[_], paint: Paint, stroke: Stroke) extends CategoryMarker(key, paint, stroke) {
  def this(key: Comparable[_], paint: Paint) = this(key, paint, new BasicStroke(1.0f))
  def this(key: Comparable[_]) = this(key, Color.gray)
}
