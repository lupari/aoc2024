package assignments

import scala.io.Source
import lib.Points.Point

object Day04:
  import lib.GridExtensions.*

  val grid: Grid[Char] =
    Source.fromResource("day04.txt").mkString.toList.toGrid.withDefaultValue('.')

  def allXmases(): Seq[String] =
    def h(p: Point)  = (1 to 3).map(x => Point(p.x + x, p.y))
    def h2(p: Point) = (1 to 3).map(x => Point(p.x - x, p.y))
    def v(p: Point)  = (1 to 3).map(y => Point(p.x, p.y + y))
    def v2(p: Point) = (1 to 3).map(y => Point(p.x, p.y - y))
    def d(p: Point)  = (1 to 3).map(xy => Point(p.x + xy, p.y + xy))
    def d2(p: Point) = (1 to 3).map(xy => Point(p.x - xy, p.y - xy))
    def d3(p: Point) = (1 to 3).map(xy => Point(p.x + xy, p.y - xy))
    def d4(p: Point) = (1 to 3).map(xy => Point(p.x - xy, p.y + xy))

    val dirs = Seq(h, h2, v, v2, d, d2, d3, d4)
    grid.keys.toList.flatMap(v => dirs.map(d => (v +: d(v)).map(grid(_)).mkString))

  def isXmas(p: Point): Boolean =
    val d1 = Seq(Point(p.x - 1, p.y - 1), p, Point(p.x + 1, p.y + 1))
    val d2 = Seq(Point(p.x + 1, p.y + 1), p, Point(p.x - 1, p.y - 1))
    val d3 = Seq(Point(p.x - 1, p.y + 1), p, Point(p.x + 1, p.y - 1))
    val d4 = Seq(Point(p.x + 1, p.y - 1), p, Point(p.x - 1, p.y + 1))
    Seq(d1, d2, d3, d4).map(ps => ps.map(p => grid(p)).mkString).count(_ == "MAS") == 2

  def partOne(): Int = allXmases().count(_ == "XMAS")
  def partTwo(): Int = grid.keys.count(isXmas)
