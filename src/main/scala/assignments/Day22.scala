package assignments

import scala.io.Source

object Day22:

  val keys: List[Long] = Source.fromResource("day22.txt").getLines.map(_.toLong).toList

  def hash(key: Long): Long =
    def step(k: Long, fn: Long => Long) = (fn(k) ^ k) % 16777216
    val key2                   = step(key, _ * 64)
    val key3                   = step(key2, _ / 32)
    step(key3, _ * 2048)

  def development(key: Long): Map[Seq[Long], Long] =
    val it       = Iterator.iterate(key, 2000)(hash).map(_ % 10).sliding(5)
    val progress = it.map(prices => prices.zip(prices.tail).map((a, b) => b - a) -> prices.last)
    progress.toSeq.groupMapReduce(_._1)(_._2)((a, _) => a)

  def partOne(): Long = keys.map(Iterator.iterate(_)(hash).drop(2000).next).sum
  def partTwo(): Long = keys.flatMap(development).groupMapReduce(_._1)(_._2)(_ + _).values.max
