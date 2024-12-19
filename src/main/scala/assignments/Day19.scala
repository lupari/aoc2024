package assignments

import scala.io.Source

object Day19:

  val input: List[String]    = Source.fromResource("day19.txt").getLines.toList
  val patterns: List[String] = input.head.split(", ").toList
  val designs: List[String]  = input.drop(2)

  def countWays(design: String): Long =
    val memo = collection.mutable.Map.empty[Int, Long]
    def helper(i: Int): Long =
      memo.getOrElseUpdate(
        i, {
          if i == design.size then 1
          else
            (for
              pattern <- patterns
              if design.startsWith(pattern, i)
            yield helper(i + pattern.size)).sum
        }
      )

    helper(0)

  val ways: List[Long] = designs.map(countWays)

  def partOne(): Int  = ways.count(_ > 0)
  def partTwo(): Long = ways.sum
