package de.sciss.coelestis

import de.sciss.mellite.Element
import de.sciss.synth.proc.Grapheme
import java.util.Date
import de.sciss.lucre.confluent
import de.sciss.lucre.confluent.{VersionPeek, VersionInfo, Cursor}
import de.sciss.lucre.synth.expr.Strings
import scala.annotation.tailrec
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.file._
import play.api.libs.json.Format
import scala.concurrent.{blocking, Await}
import scala.concurrent.duration.Duration
import scalax.chart.{ChartFactories, Charting}
import org.jfree.data.time.SimpleTimePeriod
import scala.swing.{Frame, Swing}
import scala.swing.event.WindowClosing
import org.jfree.chart.labels.{ItemLabelAnchor, ItemLabelPosition}
import org.jfree.ui.TextAnchor
import java.awt.{Rectangle, Color}
import de.sciss.pdflitz
import de.sciss.pdflitz.Generate.QuickDraw
import Swing._
import de.sciss.play.json.AutoFormat
import org.jfree.util.ShapeUtilities
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.geom.Line2D

object Analysis extends App {
  val skip      = 4000L  // milliseconds steps
  val LOG_SKIP  = false
  val LOG_PROG  = true

  generateJSON(plot())

  lazy val jsonFile = analysisDir / "audiofiles.json"

  implicit def format: Format[Vec[Command]] = {
    implicit val fmtTime    = AutoFormat[Time          ]
    implicit val fmtInfo    = AutoFormat[AudioFileInfo ]
    implicit val fmtCmd     = AutoFormat[Command       ]
    AutoFormat[Vec[Command]]
  }

  def generateJSON(done: => Unit): Unit = {
    if (jsonFile.isFile) {
      println(s"File '$jsonFile' already generated.")
      done
    } else {
      val p = audioFiles()
      // sucky executor spawns daemon threads
      new Thread {
        override def run(): Unit = {
          Await.ready(p, Duration.Inf)
          Thread.sleep(2000)
        }
        start()
      }
      p.monitor()
      p.foreach { xs =>
        println(s"\nWriting '$jsonFile'...")
        JsIO.write(xs, jsonFile)
        // quit()
        done
      }
    }
  }

  def plot(): Unit = {
    val data = JsIO.read[Vec[Command]](jsonFile).get
    // ...
    // println(s"Data.size = ${data.size}")

    import Charting._

    def label(cmd: Command): String = file(cmd.info.path).base

    val tsd = data.map { cmd =>
      val d = cmd.time.date
      println(s"${label(cmd)}${if (cmd.action == Removed) " [R]" else ""}")
      new SimpleTimePeriod(d, d) -> 0
    }

    def genLabel(ds: XYDataset, series: Int, item: Int): String = label(data(item))

    Swing.onEDT {
      val ts    = tsd.toTimePeriodValuesCollection(name = "Commands")
      val chart = ChartFactories.XYLineChart(dataset = ts, title = "Audio File Import",
        domainAxisLabel = "", legend = false)
      chart.labelGenerator = Some(genLabel _)
      val plot = chart.plot
      plot.setBackgroundPaint(Color.white)

      val yAxis = plot.getRangeAxis
      yAxis.setVisible(false)
      yAxis.setRange(-0.04, 1.0)
      val renderer = plot.getRenderer.asInstanceOf[XYLineAndShapeRenderer]
      renderer.setSeriesPositiveItemLabelPosition(0,
        // text-anchor, rotation-anchor
        new ItemLabelPosition(ItemLabelAnchor.OUTSIDE3, TextAnchor.BOTTOM_LEFT, TextAnchor.BASELINE_LEFT,
          -90.0.toRadians))
      // renderer.setItemLabelAnchorOffset(20f)
      renderer.setBaseLinesVisible(false)
      // renderer.setBaseShapesVisible(true)
      renderer.setSeriesShape(0, new Line2D.Float(0, 0, 0, 12)) // new Rectangle(0, -2, 1, 12))
        // .createLineRegion(new Line2D.Float(0f, 0f, 0f, 2f), 0.5f)) // .createDownTriangle(4f))
      // renderer.setSeriesFillPaint(0, Color.black)
      // renderer.setSeriesOutlinePaint(0, new Color(0, 0, 0, 0))
      // renderer.setBasePaint(Color.black)
      renderer.setSeriesShapesVisible(0, true)
      renderer.setSeriesPaint(0, Color.black)

      val p = chart.toPanel
      val f = new Frame {
        contents = p

        listenTo(this)
        reactions += {
          case WindowClosing(_) => quit()
        }
      }

      val w   = 1000
      val h   = 360
      val sz  = new Rectangle(0, 0, w, h)
      val draw = QuickDraw(w -> h) { g =>
        chart.peer.draw(g, sz)
      }

      new pdflitz.SaveAction(draw :: Nil).setupMenu(f)

      f.pack()
      f.centerOnScreen()
      f.open()
    }
  }

  def audioFiles(): Processor[Vec[Command], Any] = {
    val masterCursor = session.cursors.cursor
    val csr: Cursor[S, D] = masterCursor.step { implicit tx =>
      implicit val dtx: D#Tx = tx
      val existing = session.cursors.descendants.toList.collectFirst {
        case c if c.name.value == AnalysisCursor => c.cursor
      }
      existing.getOrElse {
        val cNew  = session.cursors.addChild(masterCursor.position)
        cNew.name = Strings.newVar[D](Strings.newConst(AnalysisCursor))
        cNew.cursor
      }
    }

    val firstDateT  = firstDate.getTime
    val lastDateT   = lastDate.getTime
    // var reportTime  = 0L

    //    def report(): Unit = if (LOG_PROG) {
    //      println(s"${dateFormat.format(new Date(t))} - $predPath")
    //      reportTime = System.currentTimeMillis()
    //    }

    def findNextVersion(prevTime: Long, prevPath: S#Acc, minSkip: Long = 4000)
                       (implicit tx: S#Tx): (Long, VersionInfo, S#Acc) = {
      implicit val dtx: D#Tx = tx
      val p0 = masterCursor.position

      // find interval
      @tailrec def loopFwd(t: Long, sk: Long): (Long, Long, S#Acc) = {
        val t1 = math.min(lastDateT, t + sk)
        if (LOG_SKIP) println(s"Skip >> ${dateFormat.format(new Date(t1))}")
        val p1 = p0.takeUntil(t1)
        if (t1 < lastDateT && p1 == prevPath) {
          val sk1 = if (sk < 0x80000000L) sk << 1 else sk
          loopFwd(t1, sk1)
        } else {
          (t1, sk, p1)
        }
      }

      // either pf != predPath or tf == lastDateT
      val (tf, skf, pf) = loopFwd(prevTime, minSkip)

      // binary search with the interval found
      @tailrec def loopSearch(lo: Long, hi: Long, resTime: Long, resPath: S#Acc): (Long, S#Acc) = {
        if (hi - lo <= minSkip) return (resTime, resPath)

        val mid = (lo + hi) >> 1
        if (LOG_SKIP) println(s"Skip << ${dateFormat.format(new Date(mid))}")
        val pm  = p0.takeUntil(mid)
        if (pm == prevPath) loopSearch(mid, hi, resTime, resPath) else loopSearch(lo, mid, mid, pm)
      }

      val (tb, pb) = loopSearch(tf - skf, tf, tf, pf)
      (tb, pb.info, pb)
    }

    val proc = new ProcessorImpl[Vec[Command], Any] {
      def body(): Vec[Command] = {
        var t           = firstDateT
        var predPath: S#Acc = confluent.Sys.Acc.root[S]
        var pred        = Set.empty[Grapheme.Value.Audio]
        var res         = Vec.empty[Command]

        while (t < lastDateT) {
          blocking {
            val (t1, info, p) = masterCursor.step { implicit tx => findNextVersion(t, predPath, skip) }
            val succ = csr.stepFrom(p) { implicit tx =>
              session.collectElements {
                case e: Element.AudioGrapheme[S] => e.entity.value
              } .toSet
            }

            t         = t1
            predPath  = p
            if (succ != pred) {
              val added     = succ -- pred
              val removed   = pred -- succ
              // report()
              // val pDate = new Date(info.timeStamp)
              // println(s"At ${dateFormat.format(pDate)} added $added, removed $removed")

              def audioInfo(g: Grapheme.Value.Audio): AudioFileInfo =
                AudioFileInfo(path = g.artifact.path, length = g.spec.numFrames, numChannels = g.spec.numChannels)

              val time = Time(stamp = info.timeStamp, version = VersionPeek(p))

              added.foreach { g =>
                res :+= Command(time, audioInfo(g), Added)
              }
              removed.foreach { g =>
                res :+= Command(time, audioInfo(g), Removed)
              }
              pred = succ
            }
          }
          progress((t - firstDateT).toFloat / (lastDateT - firstDateT).toFloat)
        }

        progress(1f)
        res
      }
    }
    proc.start()
    proc
  }

  def quit(): Unit = {
    println("Closing...")
    if (sessionOpen) session.system.close()
    sys.exit()
  }
}
