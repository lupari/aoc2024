package assignments

import scala.io.Source
import scala.annotation.tailrec

object Day17:
  val input: List[String] = Source.fromResource("day17.txt").getLines.toList
  val program: List[Int]  = input.last.filter(_.isDigit).map(_.asDigit).toList

  case class Registry(a: Long, b: Long = 0L, c: Long = 0L)
  def combo(i: Int, rs: Registry): Long = i match
    case 4 => rs.a
    case 5 => rs.b
    case 6 => rs.c
    case _ => i

  def execute(i: Int, ptr: Int, op: Int, rs: Registry): (Int, Option[Long], Registry) = i match
    case 0 => (ptr + 2, None, rs.copy(a = rs.a >> combo(op, rs)))
    case 1 => (ptr + 2, None, rs.copy(b = rs.b ^ op))
    case 2 => (ptr + 2, None, rs.copy(b = combo(op, rs) & 7))
    case 3 => if rs.a == 0 then (ptr + 2, None, rs) else (op, None, rs)
    case 4 => (ptr + 2, None, rs.copy(b = rs.b ^ rs.c))
    case 5 => (ptr + 2, Some(combo(op, rs) & 7), rs)
    case 6 => (ptr + 2, None, rs.copy(b = rs.a >> combo(op, rs)))
    case _ => (ptr + 2, None, rs.copy(c = rs.a >> combo(op, rs)))

  def run(program: List[Int], regs: Registry): List[Long] =
    @tailrec
    def helper(ptr: Int, rs: Registry, acc: List[Long]): List[Long] =
      if ptr + 1 >= program.length then acc
      else
        val (instr, op)      = (program(ptr), program(ptr + 1))
        val (ptr2, out, rs2) = execute(instr, ptr, op, rs)
        helper(ptr2, rs2, acc ++ out.toList)
    helper(0, regs, Nil)

  def findA(program: List[Int], ptr: Int, acc: Long): Option[Long] =
    def helper(candidate: Int): Option[Long] =
      val acc2 = acc * 8 + candidate
      if run(program, Registry(acc2)) == program.drop(ptr) then
        if ptr == 0 then Some(acc2)
        else findA(program, ptr - 1, acc2)
      else None

    (0 until 8).toList.map(helper).collectFirst { case Some(a) => a }

  def partOne(): String = run(program, Registry(input.head.filter(_.isDigit).toLong)).mkString(",")
  def partTwo(): Long   = findA(program, program.length - 1, 0L).get
