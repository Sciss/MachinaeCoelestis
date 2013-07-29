package de.sciss.coelestis

import de.sciss.desktop.impl.{WindowImpl, SwingApplicationImpl}
import de.sciss.desktop.{Window, Menu}
import jparsec.graph.chartRendering.Projection.PROJECTION
import jparsec.astronomy.TelescopeElement
import jparsec.astronomy.CoordinateSystem.COORDINATE_SYSTEM
import jparsec.observer.ObserverElement
import jparsec.time.TimeElement.SCALE
import jparsec.time.{AstroDate, TimeElement}
import jparsec.ephem.EphemerisElement
import jparsec.ephem.Target.TARGET
import jparsec.graph.SkyChart
import scala.swing.{FlowPanel, Button, BorderPanel, Swing, Component}
import jparsec.graph.chartRendering.SkyRenderElement.COLOR_MODE
import Swing._
import de.sciss.desktop.Window.Style
import jparsec.graph.chartRendering.PlanetRenderElement

object View extends SwingApplicationImpl("Machinae Coelestis") {
  lazy val menuFactory = Menu.Root()
  type Document = Unit

  object Location {
    implicit def fromObserver(obs: ObserverElement): Location =
      apply(lon = obs.getLongitudeDeg, lat = obs.getLatitudeDeg, alt = obs.getHeight)
  }
  case class Location(lon: Double, lat: Double, alt: Int) {
    def updateObserver(obs: ObserverElement): Unit = {
      obs.setLongitudeDeg(lon)
      obs.setLatitudeDeg (lat)
      obs.setHeight      (alt, false)
    }
  }

  override def init(): Unit = {
    val width     = 1024
    val height    = 768
    val planet    = PlanetRenderElement(axes = false, textures = true, mainSat = true, allSat = true, telescope = false)
    val telescope = TelescopeElement.HUMAN_EYE
    // telescope.invertHorizontal = false
    // telescope.invertVertical   = false
    val sky       = SkyRenderElement(coord = COORDINATE_SYSTEM.HORIZONTAL, proj = PROJECTION.STEREOGRAPHICAL,
      azimuth = 0.0, elevation = math.Pi * 0.5, width = width, height = height /* - 50 */,
      planet = planet, telescope = telescope)

    sky.setColorMode(COLOR_MODE.BLACK_BACKGROUND) // black on white
    sky.drawSkyBelowHorizon = false   // `false` to clip below observer's horizon
    // sky.drawOcularFieldOfView = false

    sky.drawClever = true

    val observer = new ObserverElement(Judenburg)

    val astro = new AstroDate()
    val time  = new TimeElement(astro, SCALE.LOCAL_TIME)
    val eph   = new EphemerisElement(TARGET.NOT_A_PLANET, EphemerisElement.COORDINATES_TYPE.APPARENT,
  				EphemerisElement.EQUINOX_OF_DATE, EphemerisElement.TOPOCENTRIC, EphemerisElement.REDUCTION_METHOD.IAU_2006,
  				EphemerisElement.FRAME.DYNAMICAL_EQUINOX_J2000)
    eph.preferPrecisionInEphemerides = false
    val skyRender = SkyRendering(time = time, obs = observer, eph = eph, sky = sky, title = "Sky render", marginY = 0)

    val chart     = new SkyChart(width, height /* -50 */, skyRender)

    /* val mainFrame = */ new WindowImpl {
      def handler     = View.windowHandler
      def style       = Window.Regular

      contents        = Component.wrap(chart.getComponent)
      closeOperation  = Window.CloseExit
      // pack()
      size            = (width, height)
      front()
    }

    val obsView = de.sciss.guiflitz.AutoView(observer: Location)
    obsView.cell.addListener {
      case loc =>
        loc.updateObserver(observer)
        chart.update(sky, time, observer, eph, null)  // XXX TODO: this is incredibly slow. Is this the way to go?
    }

    /* val obsFrame = */ new WindowImpl {
      def handler     = View.windowHandler
      def style       = Window.Auxiliary

      val butQuery    = Button("Query") {
        // println("Query")

        import scala.collection.JavaConversions._
        val r = skyRender.getRenderSkyObject
        val p = r.planets
        println()
        if (p != null) p .grouped(2).foreach {
          case Seq(position: Array[Float], r: PlanetRenderElement) =>
            val posx = position(0)
            val posy = position(1)
            println(s"${r.target.getName} : $posx, $posy")
        }
      }

      contents        = new BorderPanel {
        add(obsView.component, BorderPanel.Position.Center)
        add(new FlowPanel(butQuery), BorderPanel.Position.South)
      }
      closeOperation  = Window.CloseIgnore
      location        = (width, 0)
      pack()
      front()
    }

    // chart.setCentralObject("Polaris")
  }
}