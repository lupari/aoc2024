package assignments

import scala.io.Source

object Day07:

  case class Equation(test: Long, operands: List[Int]):
    def passes(concat: Boolean = false): Boolean =
      def helper(xs: List[Int], expr: List[String], acc: Long): List[List[String]] = xs match
        case Nil => if acc == test then List(expr) else Nil
        case h :: t =>
          val sums     = helper(t, expr :+ "+" :+ s"$h", acc + h)
          val products = helper(t, expr :+ "*" :+ s"$h", acc * h)
          val concats  = if concat then helper(t, expr :+ "||" :+ s"$h", s"$acc$h".toLong) else Nil
          sums ++ products ++ concats

      operands match
        case h :: t => helper(t, List(s"$h"), h).nonEmpty
        case _      => false

  def parse(s: String): Equation =
    val Seq(r, ops) = s.split(": ").toSeq
    Equation(r.toLong, ops.split("\\s+").map(_.toInt).toList)

  val input: List[Equation] = Source.fromResource("day07.txt").getLines.toList.map(parse)

  def partOne(): Long = input.filter(_.passes()).map(_.test).sum
  def partTwo(): Long = input.filter(_.passes(true)).map(_.test).sum
