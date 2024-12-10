package assignments

import scala.io.Source
import lib.Points.Point
import lib.Graphs

object Day10:
  import lib.GridExtensions.*

  val grid: Grid[Int] = Source.fromResource("day10.txt").mkString.toList.toIntGrid
  val trailheads: List[Point] = grid.filter(_._2 == 0).keys.toList

  def neighbors(p: Point): List[Point] = p.neighbors.filter(n => grid.has(n) && grid(n) - grid(p) == 1)
  def isGoal(p: Point): Boolean        = grid(p) == 9
  def trails(trailHead: Point) = Graphs.dfs.search(trailHead)(neighbors)(isGoal)
    
  def partOne(): Int = trailheads.map(trails(_).map(_.last).toSet.size).sum
  def partTwo(): Int = trailheads.map(trails(_).length).sum
