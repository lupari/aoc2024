package assignments

import scala.io.Source

object Day11:

  val stones: List[Long] =
    Source.fromResource("day11.txt").mkString.split("\\s+").map(_.toLong).toList

  def split(n: Long): Seq[Long] =
    val digits  = 1 + Math.log10(n.toDouble).toInt
    val divisor = BigInt(10).pow(digits / 2).toLong
    Seq(n / divisor, n % divisor)

  def blink(stone: Long): Seq[Long] =
    if stone == 0 then Seq(1)
    else if stone.toString.length % 2 == 0 then split(stone)
    else Seq(stone * 2024)

  def stoneCount(n: Int): Long =
    val distribution = stones.groupMapReduce(identity)(_ => 1)(_ + _).view.mapValues(_.toLong).toMap
    def step(d: Map[Long, Long]) = 
      (for {
        (stone, count) <- d.iterator
        stone2       <- blink(stone)
      } yield stone2 -> count).toList.groupMapReduce(_._1)(_._2)(_ + _)
    Iterator.iterate(distribution)(step).drop(n).next.values.sum

  def partOne(): Long = stoneCount(25)
  def partTwo(): Long = stoneCount(75)
