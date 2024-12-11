package assignments

import scala.annotation.tailrec
import scala.io.Source

object Day09:

  trait Block
  case class File(id: Int, size: Int) extends Block
  case class Space(size: Int)         extends Block

  val input: Seq[Int] = Source.fromResource("day09.txt").mkString.map(_.asDigit)
  val filesystem: List[Block] =
    input.zipWithIndex.map((c, i) => if i % 2 == 0 then File(i / 2, c) else Space(c)).toList

  def checksum(fs: List[Block]): Long =
    fs.foldLeft((0L, 0))({ case ((acc, i), block) =>
      block match
        case File(id, size) => (acc + id * (i until i + size).sum.toLong, i + size)
        case Space(size)    => (acc, i + size)
    })._1

  def defrag(fs: List[Block]): List[Block] =
    @tailrec
    def helper(fs: List[Block], acc: List[Block]): List[Block] =
      fs match
        case (file: File) :: t                 => helper(t, file +: acc)
        case (fs2 @ Space(_) +: _) :+ Space(_) => helper(fs2, acc)
        case Space(free) +: fs2 :+ (file @ File(id, fileSize)) =>
          if free == fileSize then helper(fs2, file +: acc)
          else if free > fileSize then helper(Space(free - fileSize) +: fs2, file +: acc)
          else helper(fs2 :+ File(id, fileSize - free), File(id, free) +: acc)
        case _ => acc.reverse

    helper(fs, Nil)

  def defrag2(fs: List[Block]): List[Block] =
    @tailrec
    def helper(fs: List[Block], acc: List[Block]): List[Block] =
      fs match
        case fs2 :+ (free @ Space(_)) => helper(fs2, free +: acc)
        case fs2 :+ (file @ File(_, fileSize)) =>
          val index = fs2.indexWhere {
            case Space(free) => free >= fileSize
            case File(_, _)  => false
          }
          if index >= 0 then
            val (first, last) = fs2.splitAt(index)
            val (free, after) = last match
              case (h: Space) :: t => (h.size, t)
              case _               => (-1, Nil)
            val replacement =
              if free == fileSize then List(file)
              else List(file, Space(free - fileSize))
            helper(first ++ replacement ++ after, Space(fileSize) +: acc)
          else helper(fs2, file +: acc)
        case _ => acc

    helper(fs, Nil)

  def partOne(): Long = checksum(defrag(filesystem))
  def partTwo(): Long = checksum(defrag2(filesystem))
