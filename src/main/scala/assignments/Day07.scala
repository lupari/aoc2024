package assignments

import scala.io.Source

object Day07:

  case class Equation(testValue: Long, operands: List[Int]):
    def results(concat: Boolean = false): Int =
      def helper(xs: List[Int], expr: List[String], value: Long): List[List[String]] = xs match
        case Nil => if value == testValue then List(expr) else Nil
        case h :: t =>
          val sums     = helper(t, expr :+ "+" :+ s"$h", value + h)
          val products = helper(t, expr :+ "*" :+ s"$h", value * h)
          val concats = if concat then helper(t, expr :+ "||" :+ s"$h", s"$value$h".toLong) else Nil
          sums ++ products ++ concats

      operands match
        case h :: t => helper(t, List(s"$h"), h).length
        case _      => 0

  def parse(s: String): Equation =
    val Seq(r, ops) = s.split(": ").toSeq
    Equation(r.toLong, ops.split("\\s+").map(_.toInt).toList)

  val input: Seq[Equation] = Source.fromResource("day07.txt").getLines.toList.map(parse)

  def partOne(): Long =
    input.map(e => (e.testValue, e.results())).filter(_._2 > 0).map(_._1).sum

  def partTwo(): Long =
    input.map(e => (e.testValue, e.results(true))).filter(_._2 > 0).map(_._1).sum
