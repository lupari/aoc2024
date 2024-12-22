package assignments

import scala.io.Source
import lib.Graphs.bfs
import lib.Points.Point

object Day20:
  import lib.GridExtensions.*
  val grid: Grid[Char] = Source.fromResource("day20.txt").mkString.toList.toGrid
  val (start, end): (Point, Point) = (grid.keyOf('S'), grid.keyOf('E'))
  val path: List[(Point, Int)] =
    bfs.search(start)(_.neighbors.filterNot(grid(_) == '#'))(_ == end).get._2.init.zipWithIndex

  def cheatCount()(fn: Int => Boolean): Int =
    path.flatMap { case (p1, d1) =>
      path.filter { case (p2, d2) =>
        val d12 = p1.manhattan(p2)
        fn(d12) && d1 - d2 - d12 >= 100
      }
    }.size

  def partOne(): Int = cheatCount()(_ == 2)
  def partTwo(): Int = cheatCount()(_ <= 20)