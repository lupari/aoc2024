package assignments

import scala.io.Source
import lib.Points.Point
import lib.Graphs

object Day10:
  import lib.GridExtensions.*

  val grid: Grid[Int] = Source.fromResource("day10.txt").mkString.toList.toIntGrid

  def neighbors(p: Point): Seq[Point] =
    p.neighbors.filter(n => grid.has(n) && grid(n) - grid(p) == 1)
  def isGoal(p: Point): Boolean = grid(p) == 9

  val trailheads: Seq[Point]       = grid.filter(_._2 == 0).keys.toList
  val trails: Seq[Seq[Seq[Point]]] = trailheads.map(Graphs.dfs.search(_)(neighbors)(isGoal))

  def partOne(): Int = trails.map(_.map(_.last).toSet.size).sum
  def partTwo(): Int = trails.map(_.length).sum
