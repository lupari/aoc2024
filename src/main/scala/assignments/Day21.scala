package assignments

import scala.io.Source
import lib.Points.Point

object Day21:
  import lib.StringExtensions.*
  val codes: List[String] = Source.fromResource("day21.txt").getLines.toList

  def step(src: Char, dst: Char, pad: Map[Char, Point]): String =
    val (Point(ax, ay), Point(bx, by)) = (pad(src), pad(dst))
    val (dx, dy)                       = (bx - ax, by - ay)
    val (v, h)                         = ("v" * dy + "^" * -dy, ">" * dx + "<" * -dx)
    if dx > 0 && pad.values.exists(_ == Point(ax, by)) then s"$v${h}A"
    else if pad.values.exists(_ == Point(bx, ay)) then s"$h${v}A"
    else s"$v${h}A"

  def sequences(numPaths: List[String], n: Int): List[Map[String, Long]] =
    def paths(path: String, pad: Map[Char, Point]): Map[String, Long] =
      path
        .foldLeft(("A", List.empty[String])) { case ((p, acc), c) =>
          (c.toString, acc :+ step(p.head, c, pad))
        }
        ._2
        .groupMapReduce(identity)(_ => 1L)(_ + _)

    val initial = numPaths.map(np => Map(np -> 1L))
    val pad     = " ^A\n<v>".toGrid(!_.isWhitespace)
    Iterator
      .iterate(initial)(acc =>
        acc.map(_.foldLeft(Map.empty[String, Long]) { case (newPath, (path, i)) =>
          val newVisits = paths(path, pad).map { case (k, v) => k -> (v * i) }
          newVisits.foldLeft(newPath) { case (visit, (k, v)) =>
            visit + (k -> (visit.getOrElse(k, 0L) + v))
          }
        })
      )
      .drop(n)
      .next

  def solve(n: Int): Long =
    def digits(path: String) =
      val pad = "789\n456\n123\n 0A".toGrid(!_.isWhitespace)
      path.foldLeft(("A", "")) { case ((p, acc), c) => (c.toString, acc + step(p.head, c, pad)) }._2
    def length(seq: Map[String, Long]) = seq.map((k, v) => k.length * v).sum

    sequences(codes.map(digits), n).zip(codes).map((seq, code) => length(seq) * code.init.toInt).sum

  def partOne(): Long = solve(n = 2)
  def partTwo(): Long = solve(n = 25)
