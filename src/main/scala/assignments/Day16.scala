package assignments

import scala.io.Source
import lib.Points.Point
import lib.Graphs
import lib.GridExtensions.Grid

object Day16:
  import lib.GridExtensions.*

  case class Reindeer(pos: Point, dir: Point)

  def neighbors(reindeer: Reindeer, fwd: Boolean = true): Set[(Reindeer, Int)] =
    def left(p: Point)  = Point(p.y, -p.x)
    def right(p: Point) = Point(-p.y, p.x)
    Set(
      if fwd then reindeer.copy(pos = reindeer.pos + reindeer.dir) -> 1
      else reindeer.copy(pos = reindeer.pos - reindeer.dir)        -> 1,
      reindeer.copy(dir = right(reindeer.dir)) -> 1000,
      reindeer.copy(dir = left(reindeer.dir))  -> 1000
    ).filterNot { case (r, _) => grid(r.pos) == '#' }

  val grid: Grid[Char] = Source.fromResource("day16.txt").mkString.toList.toGrid
  val start: Reindeer  = Reindeer(grid.find(_._2 == 'S').get._1, Point(1, 0))
  val end: Point       = grid.find(_._2 == 'E').get._1
  val result: (Map[Reindeer, Int], Option[(Reindeer, Int)]) =
     Graphs.dijkstra.search(start)(neighbors(_))(_.pos == end)

  def partOne(): Int = result._2.get._2
  def partTwo(): Int =
    def helper(reindeer: Reindeer): Set[Point] =
      def nfb = neighbors(_, fwd = false)
      if reindeer == start then Set(reindeer.pos)
      else
        val distance = result._1(reindeer)
        val paths = for
          (r, cost) <- nfb(reindeer)
          d         <- result._1.get(r)
          if d + cost == distance
        yield helper(r) + reindeer.pos
        paths.foldLeft(Set.empty)(_ ++ _)

    helper(result._2.get._1).size
