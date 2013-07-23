package de.sciss.coelestis

import jparsec.time.{TimeScale, TimeElement, AstroDate}
import jparsec.ephem.EphemerisElement
import jparsec.observer.{City, ObserverElement}
import java.awt.image.BufferedImage
import jparsec.util.{Logger, JPARSECException, Translate}
import jparsec.graph.DataSet
import jparsec.ephem.Target.TARGET
import jparsec.graph.chartRendering.{Graphics, SkyRenderElement, PlanetRenderElement}
import jparsec.astronomy.{Constellation, TelescopeElement}
import jparsec.astronomy.CoordinateSystem.COORDINATE_SYSTEM
import jparsec.graph.chartRendering.Projection.PROJECTION
import jparsec.graph.chartRendering.SkyRenderElement.{COLOR_MODE, MILKY_WAY_TEXTURE, REALISTIC_STARS, SUPERIMPOSED_LABELS}
import java.net.URLDecoder
import jparsec.math.Constant
import jparsec.graph.chartRendering.frame.SkyRendering
import scala.util.control.NonFatal
import jparsec.io.image.Picture
import java.awt.{Font, Color, RenderingHints}
import jparsec.examples.advanced.AnimatedGifEncoder

// quick translation of the java version included with the JPARSEC examples, (C)opyright by T. Alonso Albi
object SkyAnimation extends App {

  private var skyRender: SkyRendering = null

 	private def toTDB(time: TimeElement, obs: ObserverElement, eph: EphemerisElement): TimeElement = {
 		val jd = TimeScale.getJD(time, obs, eph, TimeElement.SCALE.BARYCENTRIC_DYNAMICAL_TIME)
 		new TimeElement(jd, TimeElement.SCALE.BARYCENTRIC_DYNAMICAL_TIME)
 	}

  // Animation for the next day of this date
  val astro   = new AstroDate()
  val year    = astro.getYear.toString
  val month   = astro.getMonth.toString
  val day     = (astro.getDay + 1).toString

  // Location and language
  val place   = "Madrid"
  val lang    = "spanish"

  // Working path
  val outputPath = "/tmp/"

  // Output gif
  val outputFileName = s"${outputPath}jparsec_skyFull_Madrid_${year}_${month}_${day}_$lang.gif"

  // Image size
  val width = 850

  try {
    val e = new AnimatedGifEncoder()
    e.start(outputFileName)
    e.setDelay(2000)
    e.setRepeat(0)
    e.setQuality(1)
    val nImg = 60*24
    val step = 60 // 1 day, 1 hour step
    for (i <- 0 to nImg by step) {
      val time = i * 24.0 / (60.0 * 24.0)
      val h     = time.toInt
      val m     = ((time-h)*60.0+0.5).toInt
      val args  = Array[String]("all", year, month, day, ""+h, ""+m, ""+width, ""+width, place, "language="+lang)
      val img   = getSky(args)

      var name  = "jparsec_skyFull_"+place+"_"+year+"_"+month+"_"+day+"_"+h+"_"+m
      name = DataSet.replaceAll(name, "-", "_", true)
      if (img == null) {
        println("Could not generate image "+name)
      } else {
        try {
          val pic = new Picture(img)
          e.addFrame(pic.getImage)
        } catch {
          case NonFatal(ex) =>
            ex.printStackTrace()
        }
        println("Added image "+name)
      }
    }
    e.finish()

    // Reduce file size automatically using gimp
    /*
    String newPath = "/home/alonso/"+FileIO.getFileNameFromPath(outputFileName);
    String gimp = "gimp -i -b \"(let* ( (image (car (file-gif-load RUN-NONINTERACTIVE \\\""+outputFileName+"\\\" \\\""+outputFileName+"\\\") ) ) ) (gimp-convert-indexed image 0 0 255 TRUE TRUE \\\"\\\") (file-gif-save RUN-NONINTERACTIVE image 0 \\\""+newPath+"\\\" \\\""+newPath+"\\\" 0 1 2000 0) )\" -b \"(gimp-quit 0)\"";
    System.out.println("Compressing gif with gimp ... (executing "+gimp+")");
    String script = "/home/alonso/gimp.sh";
    WriteFile.writeAnyExternalFile(script, gimp);
    Process p = ApplicationLauncher.executeCommand("chmod +x "+script);
    p.waitFor();
    p = ApplicationLauncher.executeCommand(script);
    p.waitFor();
    FileIO.deleteFile(script);
     */
    println("Finished!")
    sys.exit()
/*
Last step: compress using GIMP. Typical compression rate of 20%

For one image, given input and output names:
gimp -i -b "(let* ( (image (car (file-gif-load RUN-NONINTERACTIVE \"jparsec_skyFull_Madrid_2012_11_10_spanish.gif\" \"jparsec_skyFull_Madrid_2012_11_10_spanish.gif\") ) ) ) (gimp-convert-indexed image 0 0 255 TRUE TRUE \"\") (file-gif-save RUN-NONINTERACTIVE image 0 \"new.gif\" \"new.gif\" 0 1 2000 0) )" -b "(gimp-quit 0)"

For any image/s in current directory, and without the need of creating a script file:
gimp -n -i -b - <<EOF
(let* ( (file's (cadr (file-glob "*.gif" 1))) (filename "") (image 0) (layer 0) )
 (while (pair? file's)
   (set! image (car (file-gif-load RUN-NONINTERACTIVE (car file's) (car file's))))
   (gimp-convert-indexed image 0 0 255 TRUE TRUE "")
   (set! filename (string-append (substring (car file's) 0 (- (string-length (car file's)) 4)) "_new.gif"))
   (file-gif-save RUN-NONINTERACTIVE image 0 filename filename 0 1 2000 0)
   (gimp-image-delete image)
   (set! file's (cdr file's))
   )
 (gimp-quit 0)
 )
EOF

*/
  } catch {
    case NonFatal(ex) =>
      ex.printStackTrace()
  }

 	/**
 	 * For unit testing only.
 	 */
 	def getSky(arg0: Array[String]): BufferedImage =  {
 		Translate.setDefaultLanguage(Translate.LANGUAGE.ENGLISH)
    var arg = arg0
 		if (arg != null && arg.length > 0) {
 			val last = arg.last.toLowerCase
 			if (last.startsWith("language=")) {
 				val lan = last.substring(9).trim()
 				if (lan.equals("spanish")) {
 					Translate.setDefaultLanguage(Translate.LANGUAGE.SPANISH)
 				}
 				arg = DataSet.getSubArray(arg, 0, arg.length-2)
 			}
 		}

    var values = Array[Double](2011, AstroDate.MARCH, 15, 19, 15, 1000, 1000)

    var cityName = "Madrid"
 		if (arg != null && arg.length > 0) {
 			values = Array[Double](
 					arg(1).toDouble, arg(2).toDouble, arg(3).toDouble, arg(4).toDouble, arg(5).toDouble,
 					1000, 1000
      )
 			if (arg.length > 6) values(5) = arg(6).toDouble
 			if (arg.length > 7) values(6) = arg(7).toDouble
 			if (arg.length > 8) cityName  = arg(8)
 		}

 		var ts = TimeElement.SCALE.LOCAL_TIME
 		val astro = if (values(0) == 0 && values(1) == 0 && values(2) == 0 && values(3) == 0 && values(4) == 0) {
 			ts = TimeElement.SCALE.LOCAL_TIME
      new AstroDate()
 		} else {
 			new AstroDate(values(0).toInt, values(1).toInt, values(2).toInt, values(3).toInt, values(4).toInt,
 					(values(4) - values(4).toInt) * 60.0)
 		}
 		var time  = new TimeElement(astro, ts)
 		val eph   = new EphemerisElement(TARGET.NOT_A_PLANET, EphemerisElement.COORDINATES_TYPE.APPARENT,
 				EphemerisElement.EQUINOX_OF_DATE, EphemerisElement.TOPOCENTRIC, EphemerisElement.REDUCTION_METHOD.IAU_2006,
 				EphemerisElement.FRAME.DYNAMICAL_EQUINOX_J2000)
 		eph.preferPrecisionInEphemerides = false

 		val render = new PlanetRenderElement(false, true, true, false, false)
 		val telescope = TelescopeElement.BINOCULARS_7x50
 		telescope.ocular.focalLength = 400f

 		val sky = new SkyRenderElement(COORDINATE_SYSTEM.HORIZONTAL,
 				PROJECTION.STEREOGRAPHICAL, 0, 0.0, values(5).toInt, values(6).toInt, render, telescope)

 		sky.drawConstellationNamesType = Constellation.CONSTELLATION_NAME.LATIN
 		if (Translate.getDefaultLanguage == Translate.LANGUAGE.SPANISH)
 			sky.drawConstellationNamesType = Constellation.CONSTELLATION_NAME.SPANISH
 		sky.drawStarsLabels = SkyRenderElement.STAR_LABELS.ONLY_PROPER_NAME
 		if (Translate.getDefaultLanguage == Translate.LANGUAGE.SPANISH)
 			sky.drawStarsLabels = SkyRenderElement.STAR_LABELS.ONLY_PROPER_NAME_SPANISH
 		sky.drawObjectsLimitingMagnitude = 7.5f
 		sky.drawDeepSkyObjectsAllMessier = false
 		sky.drawDeepSkyObjectsOnlyMessier = true
 		sky.drawDeepSkyObjectsTextures = false
 		sky.drawPlanetsMoonSun = true
 		sky.drawSpaceProbes = false
 		sky.drawStarsGreekSymbolsOnlyIfHasProperName = false
 		sky.drawTransNeptunianObjects = false
 		sky.drawStarsLimitingMagnitude = 5.5f
 		sky.drawStarsLabelsLimitingMagnitude = sky.drawStarsLimitingMagnitude
 		sky.drawArtificialSatellites = true
 		sky.drawArtificialSatellitesOnlyThese = "ISS,HST"
 		sky.drawAsteroids = false
 		sky.drawComets = false
 		sky.drawSuperNovaEvents = false
 		sky.drawSunSpots = false
 		sky.drawDeepSkyObjectsLabels = false
 		sky.drawClever = false
 		sky.drawConstellationNamesFont = Graphics.FONT.SANS_SERIF_ITALIC_14
 		sky.drawCoordinateGridEclipticLabels = false

 		sky.drawStarsLabels = SkyRenderElement.STAR_LABELS.NONE
 		sky.drawSkyCorrectingLocalHorizon = true
 		sky.drawConstellationLimits = false
 		sky.drawFastLabels = SUPERIMPOSED_LABELS.AVOID_SUPERIMPOSING_ACCURATE
 		sky.drawFastLabelsInWideFields = false

 		sky.drawSkyBelowHorizon = false
 		sky.fillMilkyWay = true
 		sky.drawNebulaeContours = false
 		sky.drawStarsGreekSymbols = true
 		sky.drawStarsRealistic = REALISTIC_STARS.STARRED
 		sky.drawMilkyWayContoursWithTextures = MILKY_WAY_TEXTURE.OPTICAL
 		sky.setColorMode(COLOR_MODE.BLACK_BACKGROUND)
 		sky.background = 255<<24 | 0<<16 | 0<<8 | 0
 		try {
      // Ex. Madrid:
      // timeZone = 1.0, name = "Madrid", longitude = -3.6863888888888, latitude = 40.4097222222, height = 655, country = "Spain"
 			val city      = City.findCity(URLDecoder.decode(cityName, "UTF-8"))
 			val observer  = ObserverElement.parseCity(city)

 			sky.centralLongitude = 180.0 * Constant.DEG_TO_RAD
 			sky.centralLatitude = Math.PI * 0.5 //observer.latitude;

 			time = toTDB(time, observer, eph)
 			if (skyRender == null) {
 				skyRender = new SkyRendering(time, observer, eph, sky, "Sky render", 0)
 			} else {
 				skyRender.getRenderSkyObject.setSkyRenderElement(sky, time, observer, eph)
 			}

 			val offy = 0
 			skyRender.getRenderSkyObject.setYCenterOffset(0)
 			skyRender.getRenderSkyObject.ReportEphemToConsole = false
 			val bimg  = skyRender.createBufferedImage()
 			val g     = bimg.createGraphics()
 			g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
 			g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON)
 	        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
 	        g.setColor(new Color(sky.background))
 	        val cy0 = (40*sky.drawCoordinateGridFont.getSize)/15
 	        g.fillRect(0, 0, cy0, bimg.getHeight)
 	        g.fillRect(0, bimg.getHeight-cy0, bimg.getWidth, cy0+50)
 	        g.setColor(Picture.invertColor(new Color(sky.background)))
 	        g.setFont(new Font("Default", Font.PLAIN, 18))
 	        val cx = 50 + (sky.width - 50) / 2 - 5
 	        val cy = cy0 + (sky.height - cy0) / 2 - 7 + offy
 	        g.drawString("E", 25, cy)
 	        g.drawString("S", cx, bimg.getHeight-15)
 	        g.setFont(g.getFont.deriveFont((2+g.getFont.getSize).toFloat))
 	        val date = cityName+", "+astro.toMinString
 	        g.drawString(date, 50, bimg.getHeight-15)
 	        g.dispose()
 	        bimg
 		} catch {
      case NonFatal(ex) =>
        Logger.log(Logger.LEVEL.ERROR, JPARSECException.getTrace(ex.getStackTrace))
        ex.printStackTrace()
        null
    }
 	}
}