package assignments

import scala.annotation.tailrec
import scala.io.Source
import lib.Numbers.bin2dec

object Day24:
  enum Op:
    case AND, OR, XOR
  case class Gate(a: String, b: String, op: Op, out: String):
    def eval(input1: Int, input2: Int): Int =
      op match
        case Op.AND => if input1 == 1 && input2 == 1 then 1 else 0
        case Op.OR  => if input1 == 1 || input2 == 1 then 1 else 0
        case Op.XOR => if input1 != input2 then 1 else 0

  def parseInput(l: String): (String, Int) = l match
    case s"${k}: $v" => k -> v.toInt

  def parseGate(l: String): Gate = l match
    case s"$a $t $b -> $o" =>
      t match
        case "AND" => Gate(a, b, Op.AND, o)
        case "OR"  => Gate(a, b, Op.OR, o)
        case _     => Gate(a, b, Op.XOR, o)

  def simulate(wires: Map[String, Int], gates: List[Gate]): Map[String, Int] =
    @tailrec
    def helper(ws: Map[String, Int], undecided: List[Gate]): Map[String, Int] =
      if undecided.isEmpty then ws
      else
        val (evaluated, notEvaluated) =
          undecided.partition(gate => ws.contains(gate.a) && ws.contains(gate.b))
        val ws2 = evaluated.foldLeft(ws) { (w, gate) =>
          w + (gate.out -> gate.eval(w(gate.a), w(gate.b)))
        }
        helper(ws2, notEvaluated)

    helper(wires, gates)

  val input: List[String]      = Source.fromResource("day24.txt").getLines.toList
  val wiring: Map[String, Int] = input.takeWhile(!_.isBlank).map(parseInput).toMap
  val gates: List[Gate]        = input.drop(wiring.size).tail.map(parseGate)

  def partOne(): Long =
    val zs = simulate(wiring, gates).filter(_._1.head == 'z').toSeq
    bin2dec(zs.sorted.map(_._2).reverse.mkString)

//def partTwo(): Int = 0
