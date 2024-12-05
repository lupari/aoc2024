package assignments

import scala.io.Source
import lib.Graphs

object Day05:

  type Ordering = (Int, Int)
  type Paging   = List[Int]
  extension [A](a: Seq[A]) def mid: A = a.drop(a.length / 2).head

  val input: List[String] = Source.fromResource("day05.txt").getLines.filterNot(_.isEmpty).toList

  private val orderRegex = """(\d+)\|(\d+)""".r
  def parseOrdering(s: String): Ordering = s match
    case orderRegex(a, b) => (a.toInt, b.toInt)
  val ordering: Seq[Ordering]       = input.takeWhile(_.exists(_ == '|')).map(parseOrdering)

  def isOrdered(paging: Paging): Boolean =
    val positions = paging.zipWithIndex.toMap
    ordering.forall { o =>
      (positions.get(o._1), positions.get(o._2)) match
        case (Some(a), Some(b)) => a < b
        case _                  => true
    }

  def reorder(paging: Paging): Paging =
    val os = ordering.filter(o => paging.contains(o._1) && paging.contains(o._2))
    val dg = os.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    Graphs.tsort(paging, dg)

  val (ordered, unordered) =
    input.drop(ordering.length).map(_.split(',').map(_.toInt).toList).partition(isOrdered)

  def partOne(): Int = ordered.map(_.mid).sum
  def partTwo(): Int = unordered.map(reorder).map(_.mid).sum
