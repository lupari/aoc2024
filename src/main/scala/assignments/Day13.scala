package assignments

import scala.annotation.targetName
import scala.io.Source

object Day13:
  import lib.NumberExtensions.*
  case class Point(x: Long, y: Long)
  case class Machine(a: Point, b: Point, prize: Point)

  def parseMachine(ls: Seq[String]): Machine =
    val regex = """.*?(\d+)\D+(\d+)""".r
    def parse(l: String) = l match
      case regex(a, b) => Point(a.toInt, b.toInt)
    Machine(parse(ls.head), parse(ls.tail.head), parse(ls.last))

  def prizeCost(m: Machine): Option[Long] =
    for //b=(py*ax-px*ay)/(by*ax-bx*ay) a=(px-b*bx)/ax
      b <- (m.prize.y * m.a.x - m.prize.x * m.a.y) /% (m.b.y * m.a.x - m.b.x * m.a.y)
      a <- (m.prize.x - b * m.b.x) /% m.a.x
    yield 3 * a + b

  val input = Source.fromResource("day13.txt").getLines.filterNot(_.isBlank).grouped(3).toList
  val machines1: Seq[Machine] = input.map(parseMachine)
  lazy val machines2: Seq[Machine] = machines1.map(m =>
    val prize = Point(m.prize.x + 10000000000000L, m.prize.y + 10000000000000L)
    m.copy(prize = prize)
  )

  def partOne(): Long = machines1.flatMap(prizeCost).sum
  def partTwo(): Long = machines2.flatMap(prizeCost).sum
