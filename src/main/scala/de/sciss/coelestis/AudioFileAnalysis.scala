//package de.sciss.coelestis
//
//import de.sciss.mellite.Element
//import de.sciss.synth.proc.Grapheme
//import de.sciss.lucre.confluent
//import de.sciss.lucre.confluent.VersionPeek
//import de.sciss.processor.Processor
//import de.sciss.processor.impl.ProcessorImpl
//import de.sciss.file._
//import play.api.libs.json.Format
//import scala.concurrent.{blocking, Await}
//import scala.concurrent.duration.Duration
//import scalax.chart.{ChartFactories, Charting}
//import org.jfree.data.time.SimpleTimePeriod
//import scala.swing.{Frame, Swing}
//import scala.swing.event.WindowClosing
//import org.jfree.chart.labels.{ItemLabelAnchor, ItemLabelPosition}
//import org.jfree.ui.TextAnchor
//import java.awt.{Rectangle, Color}
//import de.sciss.pdflitz
//import de.sciss.pdflitz.Generate.QuickDraw
//import Swing._
//import de.sciss.play.json.AutoFormat
//import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
//import java.awt.geom.Line2D
//
//object AudioFileAnalysis extends AnalysisLike {
//  val skip      = 4000L  // milliseconds steps
//  val LOG_PROG  = true
//
//  case class AudioFileInfo(path: String, length: Long, numChannels: Int)
//
//  case class AudioFileCommand(time: Time, info: AudioFileInfo, action: Action)
//
//  def apply(): Unit = generateJSON(plot())
//
//  lazy val jsonFile = analysisDir / "audiofiles.json"
//
//  implicit def format: Format[Vec[AudioFileCommand]] = {
//    implicit val fmtTime    = AutoFormat[Time          ]
//    implicit val fmtInfo    = AutoFormat[AudioFileInfo ]
//    implicit val fmtCmd     = AutoFormat[AudioFileCommand       ]
//    AutoFormat[Vec[AudioFileCommand]]
//  }
//
//  def generateJSON(done: => Unit): Unit = {
//    if (jsonFile.isFile) {
//      println(s"File '$jsonFile' already generated.")
//      done
//    } else {
//      val p = audioFiles()
//      // sucky executor spawns daemon threads
//      new Thread {
//        override def run(): Unit = {
//          Await.ready(p, Duration.Inf)
//          Thread.sleep(2000)
//        }
//        start()
//      }
//      p.monitor()
//      p.foreach { xs =>
//        println(s"\nWriting '$jsonFile'...")
//        JsIO.write(xs, jsonFile)
//        // quit()
//        done
//      }
//    }
//  }
//
//  def plot(): Unit = {
//    val data = JsIO.read[Vec[AudioFileCommand]](jsonFile).get
//    // ...
//    // println(s"Data.size = ${data.size}")
//
//    import Charting._
//
//    def label(cmd: AudioFileCommand): String = file(cmd.info.path).base
//
//    val tsd = data.map { cmd =>
//      val d = cmd.time.date
//      println(s"${label(cmd)}${if (cmd.action == Removed) " [R]" else ""}")
//      new SimpleTimePeriod(d, d) -> 0
//    }
//
//    def genLabel(ds: XYDataset, series: Int, item: Int): String = label(data(item))
//
//    Swing.onEDT {
//      val ts    = tsd.toTimePeriodValuesCollection(name = "Commands")
//      val chart = ChartFactories.XYLineChart(dataset = ts, title = "Audio File Import",
//        domainAxisLabel = "", legend = false)
//      chart.labelGenerator = Some(genLabel _)
//      val plot = chart.plot
//      plot.setBackgroundPaint(Color.white)
//      plot.setDomainPannable(true) // SO #19281374
//
//      val yAxis = plot.getRangeAxis
//      yAxis.setVisible(false)
//      yAxis.setRange(-0.04, 1.0)
//      val renderer = plot.getRenderer.asInstanceOf[XYLineAndShapeRenderer]
//      renderer.setSeriesPositiveItemLabelPosition(0,
//        // text-anchor, rotation-anchor
//        new ItemLabelPosition(ItemLabelAnchor.OUTSIDE3, TextAnchor.BOTTOM_LEFT, TextAnchor.BASELINE_LEFT,
//          -90.0.toRadians))
//      // renderer.setItemLabelAnchorOffset(20f)
//      renderer.setBaseLinesVisible(false)
//      // renderer.setBaseShapesVisible(true)
//      renderer.setSeriesShape(0, new Line2D.Float(0, 0, 0, 12)) // new Rectangle(0, -2, 1, 12))
//        // .createLineRegion(new Line2D.Float(0f, 0f, 0f, 2f), 0.5f)) // .createDownTriangle(4f))
//      // renderer.setSeriesFillPaint(0, Color.black)
//      // renderer.setSeriesOutlinePaint(0, new Color(0, 0, 0, 0))
//      // renderer.setBasePaint(Color.black)
//      renderer.setSeriesShapesVisible(0, true)
//      renderer.setSeriesPaint(0, Color.black)
//
//      showChart(chart, w = 1000, h = 360)
//    }
//  }
//
//  def audioFiles(): Processor[Vec[AudioFileCommand], Any] = {
//    val csr = analysisCursor
//
//    val proc = new ProcessorImpl[Vec[AudioFileCommand], Any] {
//      def body(): Vec[AudioFileCommand] = {
//        var t           = firstDateT
//        var predPath: S#Acc = confluent.Sys.Acc.root[S]
//        var pred        = Set.empty[Grapheme.Value.Audio]
//        var res         = Vec.empty[AudioFileCommand]
//
//        while (t < lastDateT) {
//          blocking {
//            val (t1, info, p) = masterCursor.step { implicit tx => findNextVersion(t, predPath, skip) }
//            val succ = csr.stepFrom(p) { implicit tx =>
//              session.collectElements {
//                case e: Element.AudioGrapheme[S] => e.entity.value
//              } .toSet
//            }
//
//            t         = t1
//            predPath  = p
//            if (succ != pred) {
//              val added     = succ -- pred
//              val removed   = pred -- succ
//              // report()
//              // val pDate = new Date(info.timeStamp)
//              // println(s"At ${dateFormat.format(pDate)} added $added, removed $removed")
//
//              def audioInfo(g: Grapheme.Value.Audio): AudioFileInfo =
//                AudioFileInfo(path = g.artifact.path, length = g.spec.numFrames, numChannels = g.spec.numChannels)
//
//              val time = Time(stamp = info.timeStamp, version = VersionPeek(p))
//
//              added.foreach { g =>
//                res :+= AudioFileCommand(time, audioInfo(g), Added)
//              }
//              removed.foreach { g =>
//                res :+= AudioFileCommand(time, audioInfo(g), Removed)
//              }
//              pred = succ
//            }
//          }
//          progress((t - firstDateT).toFloat / (lastDateT - firstDateT).toFloat)
//        }
//
//        progress(1f)
//        res
//      }
//    }
//    proc.start()
//    proc
//  }
//}