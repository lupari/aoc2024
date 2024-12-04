package assignments

import scala.io.Source
import lib.Points.Point

object Day04:
  import lib.GridExtensions.*

  val grid: Grid[Char] =
    Source.fromResource("day04.txt").mkString.toList.toGrid.withDefaultValue('.')

  def words(p: Point): Seq[String] =
    p.directions.foldLeft(List.empty[String])((acc, d) =>
      acc :+ (p +: (1 to 3).map(d)).map(grid).mkString
    )

  def words2(p: Point): Seq[String] =
    val (d1, d2) = (Seq(p.upright(), p, p.downleft()), Seq(p.upleft(), p, p.downright()))
    Seq(d1, d1.reverse, d2, d2.reverse).map(_.map(grid).mkString)

  def partOne(): Int = grid.keys.toSeq.flatMap(words).count(_ == "XMAS")
  def partTwo(): Int = grid.keys.toSeq.map(words2).count(_.count(_ == "MAS") == 2)
