package assignments

import scala.io.Source

object Day02:

  val input: Seq[List[Int]] =
    Source.fromResource("day02.txt").getLines().map(_.split("\\s+").map(_.toInt).toList).toList

  def isSafe(xs: List[Int]): Boolean =
    val ascending = xs.head < xs.tail.head
    xs.sliding(2).forall { case Seq(a, b) =>
      if ascending then b > a && b - a <= 3 else a > b && a - b <= 3
    }

  def variations(xs: List[Int]): Seq[List[Int]] = xs.indices.map(xs.patch(_, Nil, 1))

  def partOne(): Int = input.count(isSafe)
  def partTwo(): Int = input.count(variations(_).exists(isSafe))
