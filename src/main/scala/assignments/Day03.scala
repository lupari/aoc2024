package assignments

import scala.io.Source

object Day03:

  val inputOne: String = Source.fromResource("day03.txt").getLines().mkString
  val inputTwo: String = Source.fromResource("day03b.txt").getLines().mkString
  
  private val mulRegex = """mul\((\d+),(\d+)\)""".r
  def parseMul(s: String): Int = s match
    case mulRegex(a, b) => a.toInt * b.toInt
  def removeDonts(s: String): String = """don't\(\).*?do\(\)""".r.replaceAllIn(s, "")

  def partOne(): Int = mulRegex.findAllIn(inputOne).map(parseMul).sum
  def partTwo(): Int = mulRegex.findAllIn(removeDonts(inputTwo)).map(parseMul).sum
