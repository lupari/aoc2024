package assignments

import scala.io.Source

object Day19:

  val input: List[String]    = Source.fromResource("day19.txt").getLines.toList
  val patterns: List[String] = input.head.split(", ").toList
  val designs: List[String]  = input.drop(2)

  def waysToMakeDesign(design: String): Long =
    val memo = collection.mutable.Map.empty[Int, Long]
    def helper(i: Int): Long =
      lazy val next =
        if i == design.length then 1
        else
          (for
            pattern <- patterns
            if design.startsWith(pattern, i)
          yield helper(i + pattern.length)).sum
      memo.getOrElseUpdate(i, next)

    helper(0)

  val ways: List[Long] = designs.map(waysToMakeDesign)

  def partOne(): Int  = ways.count(_ > 0)
  def partTwo(): Long = ways.sum
