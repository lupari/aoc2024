package assignments

import scala.io.Source
import lib.Points.*

object Day15:
  import lib.GridExtensions.*

  val input: String     = Source.fromResource("day15.txt").mkString
  val grid: Grid[Char]  = input.split("\n\n").head.toList.toGrid
  val moves: Seq[Point] = input.split("\n\n").last.flatMap(Position.directions.get)

  def move1(g: Grid[Char], robot: Point, dir: Point): (Grid[Char], Point) =
    val robot2 = robot + dir
    val box    = Iterator.iterate(robot2)(_ + dir).find(g(_) != 'O').get
    if g(box) == '.' then (g.updated(box, 'O').updated(robot2, '.'), robot2)
    else (g, robot)

  def move2(g: Grid[Char], robot: Point, dir: Point): (Grid[Char], Point) =
    def helper(acc: Grid[Char], pos: Point): Option[Grid[Char]] =
      acc(pos) match
        case '.' => Some(acc)
        case '#' => None
        case '[' if dir.x == 0 =>
          val pos2 = pos + dir
          for
            g2 <- helper(acc, pos2)
            g3 <- helper(g2, pos2 + Point(1, 0))
          yield g3
            .updated(pos2, '[')
            .updated(pos2 + Point(1, 0), ']')
            .updated(pos, '.')
            .updated(pos + Point(1, 0), '.')
        case ']' if dir.x == 0 => helper(acc, pos - Point(1, 0))
        case block @ ('[' | ']') if dir.y == 0 =>
          val pos2 = pos + dir
          for g2 <- helper(acc, pos2)
          yield g2.updated(pos2, block).updated(pos, '.')

    val robot2 = robot + dir
    helper(g, robot2) match
      case Some(g2) => (g2, robot2)
      case None     => (g, robot)

  def coordinateSum(g: Grid[Char], box: Char)(
      move: (Grid[Char], Point, Point) => (Grid[Char], Point)
  ): Int =
    val robot0       = g.find(_._2 == '@').get._1
    val g0           = g.updated(robot0, '.')
    val (g1, robot1) = moves.foldLeft((g0, robot0))((acc, m) => move(acc._1, acc._2, m))
    g1.filter(_._2 == box).map(x => 100 * x._1.y + x._1.x).sum

  def partOne(): Int = coordinateSum(grid, 'O')(move1)

  def partTwo(): Int =
    def transform(c: Char): Seq[Char] = c match
      case 'O' => Seq('[', ']')
      case '@' => Seq('@', '.')
      case _   => Seq(c, c)

    val g2 = grid.flatMap((k, v) =>
      transform(v).zipWithIndex.map((v2, i) => k.copy(x = k.x * 2 + i) -> v2)
    )

    coordinateSum(g2, '[')(move2)
