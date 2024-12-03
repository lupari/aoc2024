package assignments

import scala.io.Source

object Day03:

  val input: String = Source.fromResource("day03.txt").getLines.mkString

  private val mulRegex = """mul\((\d+),(\d+)\)""".r
  def parseMul(s: String): Int = s match
    case mulRegex(a, b) => a.toInt * b.toInt
  def removeDonts(s: String): String = """don't\(\).*?do\(\)""".r.replaceAllIn(s, "")

  def partOne(): Int = mulRegex.findAllIn(input).map(parseMul).sum
  def partTwo(): Int = mulRegex.findAllIn(removeDonts(input)).map(parseMul).sum
