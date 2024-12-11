package assignments

import scala.io.Source
import lib.Graphs.floodfill
import lib.Points.Point

object Day12:
  import lib.GridExtensions.*
  val garden: Grid[Char] =
    Source.fromResource("day12.txt").mkString.toList.toGrid.withDefaultValue('#')

  type Region = Set[Point]

  def perimeter(region: Region): Seq[(Point, Point)] =
    def boundaries(p: Point): Set[(Point, Point)] =
      Set((p.above(), p), (p.left(), p), (p, p.right()))
    val pairs = region.toList.flatMap(boundaries)
    pairs.groupMapReduce(identity)(_ => 1)(_ + _).toList.filter(_._2 == 1).map(_._1)

  def sides(region: Region): Int =
    def groupCount(g: Seq[Int]) = 1 + g.sorted.sliding(2).count {
      case Seq(a, b) => b - 1 > a
      case _         => false
    }
    val p  = perimeter(region)
    val lr = p.filter((a, b) => a.y == b.y).map(_._1).groupMap(p => (p.x, region.contains(p)))(_.y)
    val ud = p.filter((a, b) => a.x == b.x).map(_._1).groupMap(p => (p.y, region.contains(p)))(_.x)
    ud.values.map(groupCount).sum + lr.values.map(groupCount).sum

  def nf(p: Point): Seq[Point] = p.neighbors.filter(n => garden(n) == garden(p))
  val regions: Seq[Region]     = garden.keys.map(floodfill(_, nf)(_ => true)).toList

  def partOne(): Int = regions.map(r => perimeter(r).size * r.size).sum
  def partTwo(): Int = regions.map(r => sides(r) * r.size).sum
