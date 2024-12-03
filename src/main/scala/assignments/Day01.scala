package assignments

import scala.io.Source

object Day01:

  val (lefts, rights) = Source
    .fromResource("day01.txt")
    .getLines
    .map(_.split("\\s+"))
    .map(xs => (xs.head.toInt, xs.last.toInt))
    .toList
    .unzip

  def partOne(): Int = lefts.sorted.zip(rights.sorted).map(t => (t._1 - t._2).abs).sum
  def partTwo(): Int = lefts.map(l => rights.count(_ == l) * l).sum
