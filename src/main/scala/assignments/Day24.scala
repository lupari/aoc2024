package assignments

import scala.annotation.tailrec
import scala.io.Source
import lib.Numbers

object Day24:
  case class Gate(a: String, b: String, op: String, out: String):
    def eval(i1: Int, i2: Int): Int = op match
      case "AND" => i1 & i2
      case "OR"  => i1 | i2
      case "XOR" => i1 ^ i2

  def parseInput(l: String): (String, Int) = l match
    case s"${k}: $v" => k -> v.toInt

  def parseGate(l: String): Gate = l match
    case s"$a $op $b -> $out" => Gate(a, b, op, out)

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
    Numbers.bin2dec(zs.sorted.map(_._2).reverse.mkString)

//def partTwo(): Int = 0
