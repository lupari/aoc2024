package assignments

import scala.io.Source
import lib.Points.Point

object Day04:
  import lib.GridExtensions.*

  val grid: Grid[Char] =
    Source.fromResource("day04.txt").mkString.toList.toGrid.withDefaultValue('.')

  def words(p: Point) =
    p.directions.foldLeft(List[String]())((acc, d) =>
      acc :+ (p +: (1 to 3).map(d)).map(grid(_)).mkString
    )

  def isXmas(p: Point): Boolean =
    val d1 = Seq(Point(p.x - 1, p.y - 1), p, Point(p.x + 1, p.y + 1))
    val d2 = Seq(Point(p.x + 1, p.y + 1), p, Point(p.x - 1, p.y - 1))
    val d3 = Seq(Point(p.x - 1, p.y + 1), p, Point(p.x + 1, p.y - 1))
    val d4 = Seq(Point(p.x + 1, p.y - 1), p, Point(p.x - 1, p.y + 1))
    Seq(d1, d2, d3, d4).map(_.map(grid(_)).mkString).count(_ == "MAS") == 2

  def partOne(): Int = grid.keys.toList.flatMap(words).count(_ == "XMAS")
  def partTwo(): Int = grid.keys.count(isXmas)
