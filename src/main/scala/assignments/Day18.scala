package assignments

import scala.io.Source
import lib.Points.Point
import lib.Graphs
import scala.annotation.tailrec

object Day18:

  def parse(l: String) = l match
    case s"$x,$y" => Point(x.toInt, y.toInt)
  val bytes = Source.fromResource("day18.txt").getLines.map(parse).toList
  val start = Point(0,0)
  val end = Point(70,70)

  def withinBounds(p: Point) = p.x >= 0 && p.y >= 0 && p.x < end.x+1 && p.y < end.y+1
  def search(nf: Point => Set[Point]) = Graphs.bfs.search(start)(nf)(_ == end)

  def findFirstBlock(xs: List[Point]): Point = 
    @tailrec
    def helper(lower: Int, upper: Int): Point =
      if lower >= upper then xs(lower-1)
      else
        val mid = (lower + upper) / 2
        val xs1 = xs.take(mid)
        def nf(p: Point) = p.neighbors.filter(n => withinBounds(n) && !xs1.contains(n)).toSet
        search(nf) match
          case None => helper(lower, mid - 1)
          case Some(_) => helper(mid + 1, upper)

    helper(1, xs.length + 1)

  def partOne(): Int = 
    val kiloByte = bytes.take(1024)
    def nf(p: Point) = p.neighbors.filter(n => withinBounds(n) && !kiloByte.contains(n)).toSet
    search(nf).get

  def partTwo(): String = 
    val point = findFirstBlock(bytes)
    s"${point.x},${point.y}"