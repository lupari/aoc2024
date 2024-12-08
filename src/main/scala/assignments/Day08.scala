package assignments

import scala.io.Source
import lib.Points.Point

object Day08:
  import lib.GridExtensions.*
  import lib.IteratorExtensions.*
  val grid: Grid[Char] = Source.fromResource("day08.txt").mkString.toList.toGrid

  def antinodes1(p1: Point, p2: Point): Set[Point] =
    val d = p2 - p1
    Set(p1 - d, p2 + d).intersect(grid.keySet)

  def antinodes2(p1: Point, p2: Point): Set[Point] =
    val d = p2 - p1
    def points(dir: Int) = Set.unfold(p1)(p =>
      val next = p + d * dir
      if grid.has(next) then Some((next, next)) else None
    )
    points(1) ++ points(-1) + p1 + p2

  def antinodeCount(fn: ((Point, Point)) => Set[Point]): Int =
    val antennas = grid.filter(_._2 != '.').groupMap(_._2)(_._1)
    val pairs    = antennas.view.mapValues(_.allPairs).values
    pairs.flatMap(_.map(fn)).flatten.toSet.size

  def partOne(): Int = antinodeCount(antinodes1)
  def partTwo(): Int = antinodeCount(antinodes2)
