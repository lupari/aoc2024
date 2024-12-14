package assignments

import scala.io.Source
import lib.Points.Point

object Day14:
  import lib.NumberExtensions.*
  val (w, h) = (101, 103)

  case class Robot(pos: Point, velocity: Point):
    def move: Robot =
      copy(pos = Point((pos.x + velocity.x) %+ w, (pos.y + velocity.y) %+ h))

  def parse(s: String): Robot = s match
    case s"p=$px,$py v=$vx,$vy" => Robot(Point(px.toInt, py.toInt), Point(vx.toInt, vy.toInt))
  val robots: Seq[Robot] = Source.fromResource("day14.txt").getLines().toList.map(parse)

  def partOne(): Int =
    val moved                = Iterator.iterate(robots)(_.map(_.move)).drop(100).next
    val relevant             = moved.filterNot(r => r.pos.x == w / 2 || r.pos.y == h / 2)
    val (l, r)               = relevant.partition(_.pos.x < w / 2)
    val ((tl, bl), (tr, br)) = (l.partition(_.pos.y < h / 2), r.partition(_.pos.y < h / 2))
    Seq(tl, bl, tr, br).map(_.size).product

  def partTwo(): Int =
    val it = Iterator.iterate(robots)(_.map(_.move)).zipWithIndex
    it.find(_._1.groupMapReduce(_.pos)(_ => 1)(_ + _).values.forall(_ <= 1)).get._2
