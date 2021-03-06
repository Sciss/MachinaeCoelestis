package de.sciss

import java.awt.{Color, Font}
import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import _root_.play.api.libs.json.{Format, JsError, JsNumber, JsObject, JsResult, JsSuccess, JsValue}
import de.sciss.file._
import de.sciss.mellite.{ConfluentDocument, Document}
import de.sciss.processor.Processor
import de.sciss.synth.Curve
import de.sciss.synth.proc.Confluent
import org.jfree.chart.plot.{CategoryPlot, Plot, XYPlot}
import org.jfree.chart.renderer.xy.{StandardXYBarPainter, XYBarRenderer}
import scalax.chart.Chart

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable
import scala.concurrent.ExecutionContext
import scala.concurrent.stm.Txn
import scala.language.higherKinds
import scala.util.{Failure, Success}

package object coelestis {
  type S = Confluent
  type D = S#D

  lazy val desktop    : File = userHome / "Desktop"
  lazy val sessionFile: File = desktop / "MachinaeCoelestis" / "mellite" / "MachinaeCoelestis.mllt"
  // lazy val sessionFile2 = desktop / "Indeterminus" / "Indeterminus.mllt"

  lazy val firstDate  : Date = "2013-08-16 17:10:27".toDate
  lazy val lastDate   : Date = "2013-08-27 01:44:54".toDate
  lazy val firstDateT : Long = firstDate.getTime
  lazy val lastDateT  : Long = lastDate .getTime

  lazy val machinaeRegionsFile: File = analysisDir / "machinae_regions.json"
  def indetFile(it: Int)      : File = analysisDir / s"Indeterminus_regions$it.json"

  lazy val indetSplits = Map(1 -> 1369690322061L, 2 -> 1369740292685L, 3 -> 1369855011120L)

  private val sync          = new AnyRef
  private var _sessionOpen  = false

  def sessionOpen: Boolean = _sessionOpen

  lazy val session: ConfluentDocument = {
    sync.synchronized(_sessionOpen = true)
    Document.read(sessionFile)
  }

  lazy val dateFormat   = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.US)

  lazy val analysisDir  = new File("analysis")

  implicit class RichString(s: String) {
    def toDate: Date = dateFormat.parse(s)
  }

  val AnalysisCursor = "Analysis"

  case class Time(stamp: Long, version: Int) {
    lazy val date = new Date(stamp)

    override def toString = s"$productPrefix($date, version = $version)"
  }

  val  Vec: immutable.IndexedSeq.type = collection.immutable.IndexedSeq
  type Vec[+A]                        = collection.immutable.IndexedSeq[A]

  implicit def executionContext: ExecutionContext = ExecutionContext.global

  implicit class RichProcessor(proc: Processor[Any, _]) {
    def monitor(printResult: Boolean = false, barLength: Int = 50): Unit = {
      var lastProg = 0
      proc.addListener {
        case prog @ Processor.Progress(_, _) =>
          val p = prog.toInt * barLength/100
          while (lastProg < p) {
            print('#')
            lastProg += 1
          }
      }

      proc.onComplete {
        case Failure(Processor.Aborted()) =>
          println(s" Aborted $proc")

        case Failure(err) =>
          println(s" Failure: $proc")
          err.printStackTrace()

        case Success(_) =>
          println(if (printResult) s" Result: $proc" else " Ok.")
      }
    }
  }

  def requireNoTxn(): Unit = require(Txn.findCurrent.isEmpty, "Must be called outside of Txn")

  implicit object CurveFormat extends Format[Curve] {
    def reads(json: JsValue): JsResult[Curve] = json match {
      case JsObject(fields) =>
        val map = fields.toMap
        map.get("id") match {
          case Some(JsNumber(idj)) =>
            val id = idj.toInt
            id match {
              case Curve.parametric.id =>
                map.get("curvature") match {
                  case Some(JsNumber(x)) => JsSuccess(Curve.parametric(x.toFloat))
                  case _ => JsError(s"Field 'curvature' not found in $fields")
                }
              case Curve.step       .id => JsSuccess(Curve.step)
              case Curve.linear     .id => JsSuccess(Curve.linear)
              case Curve.exponential.id => JsSuccess(Curve.exponential)
              case Curve.sine       .id => JsSuccess(Curve.sine)
              case Curve.welch      .id => JsSuccess(Curve.welch)
              case Curve.squared    .id => JsSuccess(Curve.squared)
              case Curve.cubed      .id => JsSuccess(Curve.cubed)
              case other               => JsError(s"Unexpected envelope shape ID $other")
            }

          case _ => JsError(s"Field 'id' not found in $fields")
        }

      case other => JsError(s"Not a JSON object: $other")
    }

    def writes(c: Curve): JsValue = {
      val f0 = ("id" -> JsNumber(c.id)) :: Nil
      val f1 = c match {
        case Curve.parametric(x) => ("curvature" -> JsNumber(x.toDouble)) :: f0
        case _ => f0
      }
      JsObject(f1)
    }
  }

  private final class GroupWithIterator[A, CC[~] <: Iterable[~], To](it: CC[A], p: (A, A) => Boolean)
                                                                (implicit cbf: CanBuildFrom[CC[A], A, To])
    extends Iterator[To] {

    private val peer      = it.iterator
    private var consumed  = true
    private var elem      = null.asInstanceOf[A]

    def hasNext: Boolean = !consumed || peer.hasNext

    private def pop(): A = {
      if (!consumed) return elem
      if (!peer.hasNext) throw new NoSuchElementException("next on empty iterator")
      val res   = peer.next()
      elem      = res
      consumed  = false
      res
    }

    def next(): To = {
      val b = cbf()

      @tailrec def loop(pred: A): Unit = {
        b       += pred
        consumed = true
        if (peer.hasNext) {
          val succ = pop()
          if (p(pred, succ)) loop(succ)
        }
      }

      loop(pop())
      b.result()
    }
  }

  implicit final class RichIterableLike[A, CC[~] <: Iterable[~]](val it: CC[A]) extends AnyVal {
    /** Produces a map from the input elements to the frequency in which they appear in the input collection.
      *
      * For example:
      * {{
      *   val x = List("a", "a", "b", "a")
      *   val m = x.counted
      * }}
      *
      * produces `Map("a" -> 3, "b" -> 1)`. The map has a default value of zero,
      * so calling `m("c")` returns zero.
      *
      * @return a map with the elements counted.
      */
    def counted: Map[A, Int] = (Map.empty[A, Int].withDefaultValue(0) /: it)((m, e) => m.updated(e, m(e) + 1))

    /** Clumps the collection into groups based on a predicate which determines if successive elements
      * belong to the same group.
      *
      * For example:
      * {{
      *   val x = List("a", "a", "b", "a", "b", "b")
      *   x.groupWith(_ == _).to[Vector]
      * }}
      *
      * produces `Vector(List("a", "a"), List("b"), List("a"), List("b", "b"))`.
      *
      * @param p    a function which is evaluated with successive pairs of the input collection. As long
      *             as the predicate holds (the function returns `true`), elements are lumped together.
      *             When the predicate becomes `false`, a new group is started.
      *
      * @param cbf  a builder factory for the group type
      * @tparam To  the group type
      * @return     an iterator over the groups.
      */
    def groupWith[To](p: (A, A) => Boolean)(implicit cbf: CanBuildFrom[CC[A], A, To]): Iterator[To] =
      new GroupWithIterator(it, p)

    def meanVariance(implicit num: Fractional[A]): (A, A) = {
      var sum   = num.zero
      var size  = num.zero
      val one   = num.one
      import num.mkNumericOps
      it.foreach { e =>
        sum  += e
        size += one
      }
      val mean = sum / size
      var vari = num.zero
      it.foreach { e =>
        val d = e - mean
        vari += d * d
      }

      (mean, vari)
    }
  }

  implicit final class RichIndexedSeq[A](val sq: IndexedSeq[A]) extends AnyVal {
    /** Nearest percentile (rounded index, no interpolation). */
    def percentile(n: Int): A = sq((sq.size * n - 50) / 100)
  }

  private lazy val defaultFontFace = "Helvetica"  // "Arial"

  implicit class RichChart[P <: Plot](chart: Chart[P]) {
    /** Adjust the chart with a black-on-white color scheme and
      * fonts that come out properly in PDF export.
      */
    def printableLook(): Unit = {
      val plot = chart.plot

      val (xAxis, yAxis) = plot match {  // shitty Plot / Renderer interfaces do not have common super types
        case p: XYPlot       =>
          p.setBackgroundPaint           (Color.white    )
          p.setDomainGridlinePaint       (Color.lightGray)
          p.setRangeGridlinePaint        (Color.lightGray)
          p.getRenderer.setSeriesPaint(0, Color.darkGray )
          // undo the crappy "3D" look
          p.getRenderer match {
            case r: XYBarRenderer => r.setBarPainter(new StandardXYBarPainter())
            case _ =>
          }
          (p.getDomainAxis, p.getRangeAxis)
        case p: CategoryPlot =>
          p.setBackgroundPaint           (Color.white    )
          p.setDomainGridlinePaint       (Color.lightGray)
          p.setRangeGridlinePaint        (Color.lightGray)
          p.getRenderer.setSeriesPaint(0, Color.darkGray )
          // undo the crappy "3D" look
          p.getRenderer match {
            case r: XYBarRenderer => r.setBarPainter(new StandardXYBarPainter())
            case _ =>
          }
          (p.getDomainAxis, p.getRangeAxis)
      }

      //      val xAxis         = plot.getDomainAxis
      //      val yAxis         = plot.getRangeAxis
      val fnt1          = new Font(defaultFontFace, Font.BOLD , 14)
      val fnt2          = new Font(defaultFontFace, Font.PLAIN, 12)
      xAxis.setLabelFont(fnt1)
      xAxis.setTickLabelFont(fnt2)
      yAxis.setLabelFont(fnt1)
      yAxis.setTickLabelFont(fnt2)
    }
  }

//  implicit class RichTuple3sMore[A,B,C](it: Iterable[(A,B,C)]) {
//    def toXYSeries(name: Comparable[_] = "", autoSort: Boolean = true, allowDuplicateXValues: Boolean = true)
//      (implicit eva: A ⇒ Number, evb: B ⇒ Number): XYZSeries = {
//
//      val series = new XYZSeries(name, autoSort, allowDuplicateXValues)
//      it foreach { case (x,y) ⇒ series.add(x,y) }
//      series
//    }
//
//    def toXYZDataset(implicit eva: A => Comparable[A], evb: B => Comparable[B], evc: C => Number): XYZDataset = {
//      val dataset = new DefaultXYZDataset
//      it.foreach { case (x ,y, z) => dataset.addSeries()
//      dataset
//    }
//
//  }
}