package assignments

import scala.io.Source
import lib.Graphs.bfs
import lib.Points.Point

object Day20:
  import lib.GridExtensions.*
  val grid: Grid[Char] = Source.fromResource("day20.txt").mkString.toList.toGrid
  val start: Point     = grid.find(_._2 == 'S').get._1
  val end: Point       = grid.find(_._2 == 'E').get._1
  val path: List[(Point, Int)] =
    bfs.search(start)(_.neighbors.filterNot(grid(_) == '#'))(_ == end).get._2.init.zipWithIndex

  def cheat()(fn: Int => Boolean): Int =
    path.flatMap { case (p1, d1) =>
      path.filter { case (p2, d2) =>
        val d = p1.manhattan(p2)
        fn(d) && d1 - d2 - d >= 100
      }
    }.size

  def partOne(): Int = cheat()(_ == 2)
  def partTwo(): Int = cheat()(_ <= 20)
