package de.sciss

import jparsec.observer.{ObserverElement, CityElement}
import jparsec.graph.chartRendering.{SkyRenderElement, Projection, PlanetRenderElement}
import jparsec.astronomy.{CoordinateSystem, TelescopeElement}
import jparsec.ephem.EphemerisElement
import jparsec.time.TimeElement
import jparsec.graph.chartRendering.frame.SkyRendering

package object coelestis {
  val Judenburg = new CityElement("Judenburg", 14.6612, 47.1678, 1.0, 727) // name, lon, lat, time-zone, alt

  // constructor methods providing named arguments support

  def PlanetRenderElement(axes: Boolean, textures: Boolean, mainSat: Boolean, allSat: Boolean,
                          telescope: Boolean): PlanetRenderElement =
    new PlanetRenderElement(axes, textures, mainSat, allSat, telescope)

	def SkyRenderElement(coord: CoordinateSystem.COORDINATE_SYSTEM, proj: Projection.PROJECTION, azimuth: Double,
                       elevation: Double, width: Int, height: Int, planet: PlanetRenderElement,
                       telescope: TelescopeElement): SkyRenderElement =
    new SkyRenderElement(coord, proj, azimuth, elevation, width, height, planet, telescope)

	def SkyRendering(time: TimeElement, obs: ObserverElement, eph: EphemerisElement, sky: SkyRenderElement,
			title: String, marginY: Int): SkyRendering = new SkyRendering(time, obs, eph, sky, title, marginY)
}
