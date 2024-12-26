package assignments

import scala.io.Source

object Day25:

  val input: List[Seq[String]] = Source.fromResource("day25.txt").getLines.filterNot(_.isBlank).grouped(7).toList
  val (locks, keys) = input.partition(_.head.forall(_ == '#'))

  def heights(xs: Seq[String]): Seq[Int] = xs.transpose.map(_.count(_ == '#') - 1)

  def partOne(): Int =
    (for
      key <- keys.map(heights); lock <- locks.map(heights)
      if key.zip(lock).forall((k, l) => k + l <= 5)
    yield ()).size
