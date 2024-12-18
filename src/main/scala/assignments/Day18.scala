package assignments

import scala.annotation.tailrec
import scala.io.Source
import lib.Points.Point
import lib.Graphs
import lib.Search

object Day18:

  def parse(l: String): Point = l match
    case s"$x,$y" => Point(x.toInt, y.toInt)
  val bytes: List[Point]           = Source.fromResource("day18.txt").getLines.map(parse).toList
  val (start, end): (Point, Point) = (Point(0, 0), Point(70, 70))

  def isOpen(p: Point, bytes: Seq[Point]): Boolean = p <=> end && !bytes.contains(p)
  def search()(nf: Point => Set[Point]): Option[Int] = Graphs.bfs.search(start)(nf)(_ == end)

  def partOne(): Int = search()(_.neighbors.filter(isOpen(_, bytes.take(1024))).toSet).get
  def partTwo(): String = 
    def hasPath(xs: Iterable[Point]) = search()(_.neighbors.filter(isOpen(_, xs.toSeq)).toSet)
    Search.binSearch(bytes)(hasPath)._1.last.toString
