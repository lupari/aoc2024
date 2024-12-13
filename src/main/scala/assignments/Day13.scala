package assignments

import scala.io.Source
import lib.Points.Point

import scala.annotation.targetName

object Day13:

  val input =
    Source.fromResource("day13.txt").getLines().toList.grouped(4).map(_.filterNot(_.isBlank)).toList
  case class PointL(x: Long, y: Long)
  case class Machine(a: Point, b: Point, prize: PointL)

  extension (n: Long)
    @targetName("divmod")
    def %/(d: Long): Option[Long] = if (n % d == 0) Some(n / d) else None

  def parseMachine(ls: List[String]): Machine =
    val regex = """.*?(\d+)\D+(\d+)""".r
    def parse(l: String) = l match
      case regex(a, b) => Point(a.toInt, b.toInt)
    def parsePrize(l: String) = l match
      case regex(a, b) => PointL(a.toLong, b.toLong)
    Machine(parse(ls.head), parse(ls.tail.head), parsePrize(ls.last))

  def prizeCost(m: Machine): Option[Long] =
    for
      t1 <- (m.prize.x * m.a.y - m.prize.y * m.a.x) %/ (m.b.x * m.a.y - m.b.y * m.a.x)
      t2 <- (m.prize.x - t1 * m.b.x) %/ m.a.x
    yield 3 * t2 + t1

  val machines1: Seq[Machine] = input.map(parseMachine)
  lazy val machines2: Seq[Machine] = machines1.map(m =>
    val prize = m.prize.copy(x = m.prize.x + 10000000000000L, y = m.prize.y + 10000000000000L)
    m.copy(prize = prize)
  )

  def partOne(): Long = machines1.flatMap(prizeCost).sum
  def partTwo(): Long = machines2.flatMap(prizeCost).sum
