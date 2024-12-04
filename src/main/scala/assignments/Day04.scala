package assignments

import scala.io.Source
import lib.Points.Point

object Day04:
  import lib.GridExtensions.*

  val grid: Grid[Char] =
    Source.fromResource("day04.txt").mkString.toList.toGrid.withDefaultValue('.')

  def allXmases(): Seq[String] =
    val r                     = (1 to 3)
    def horizontal(p: Point)  = (p +: r.map(x => Point(p.x + x, p.y)))
    def horizontal2(p: Point) = (p +: r.map(x => Point(p.x - x, p.y)))
    def vertical(p: Point)    = (p +: r.map(y => Point(p.x, p.y + y)))
    def vertical2(p: Point)   = (p +: r.map(y => Point(p.x, p.y - y)))
    def diagonal(p: Point)    = (p +: r.map(xy => Point(p.x + xy, p.y + xy)))
    def diagonal2(p: Point)   = (p +: r.map(xy => Point(p.x - xy, p.y - xy)))
    def diagonal3(p: Point)   = (p +: r.map(xy => Point(p.x + xy, p.y - xy)))
    def diagonal4(p: Point)   = (p +: r.map(xy => Point(p.x - xy, p.y + xy)))

    val dirs =
      Seq(horizontal, horizontal2, vertical, vertical2, diagonal, diagonal2, diagonal3, diagonal4)
    grid.keys.toList.flatMap(v => dirs.map(_.apply(v).map(grid(_)).mkString))

  def isXmas(p: Point): Boolean =
    val d1 = List(Point(p.x - 1, p.y - 1), p, Point(p.x + 1, p.y + 1)).map(grid(_)).mkString
    val d2 = List(Point(p.x + 1, p.y + 1), p, Point(p.x - 1, p.y - 1)).map(grid(_)).mkString
    val d3 = List(Point(p.x - 1, p.y + 1), p, Point(p.x + 1, p.y - 1)).map(grid(_)).mkString
    val d4 = List(Point(p.x + 1, p.y - 1), p, Point(p.x - 1, p.y + 1)).map(grid(_)).mkString
    Seq(d1, d2, d3, d4).count(_ == "MAS") == 2

  def partOne(): Int = allXmases().count(_ == "XMAS")
  def partTwo(): Int = grid.keys.count(isXmas)
