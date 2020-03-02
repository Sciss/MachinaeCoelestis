package de.sciss.coelestis

import java.awt.{BasicStroke, Color, Paint, Stroke}

import org.jfree.chart.plot.CategoryMarker

class LeftCategoryMarker(key: Comparable[_], paint: Paint, stroke: Stroke) extends CategoryMarker(key, paint, stroke) {
  def this(key: Comparable[_], paint: Paint) = this(key, paint, new BasicStroke(1.0f))
  def this(key: Comparable[_]) = this(key, Color.gray)
}
