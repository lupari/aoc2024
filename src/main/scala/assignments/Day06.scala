package assignments

import scala.io.Source
import scala.annotation.tailrec
import lib.Points.*

object Day06:
  import lib.GridExtensions.*

  val grid: Grid[Char] = Source.fromResource("day06.txt").mkString.toList.toGrid
  val start: Dir       = Dir(grid.find(_._2 == '^').get._1, 'U')

  def walk(g: Grid[Char]): Set[Point] =
    @tailrec
    def helper(acc: Set[Dir], dir: Dir): Set[Point] =
      if acc.contains(dir) then Set.empty
      else
        g.get(dir.forward().p) match
          case Some(n) => helper(acc + dir, if n == '#' then dir.rotate() else dir.forward())
          case _       => (acc + dir).map(_.p)

    helper(Set.empty, start)

  val path: Set[Point] = walk(grid)

  def partOne(): Int = path.size
  def partTwo(): Int =
    grid.keys.filter(path.contains).toList.map(o => walk(grid + (o -> '#'))).count(_.isEmpty)
