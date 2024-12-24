package assignments

import scala.io.Source
import lib.Numbers.bin2dec

import scala.annotation.tailrec

object Day24:
  case class Gate(left: String, right: String, op: String, out: String):
    def eval(a: Int, b: Int) = op match
      case "AND" => if a == 1 && b == 1 then 1 else 0
      case "OR"  => if a == 1 || b == 1 then 1 else 0
      case _ => if a != b then 1 else 0

  def parseInput(l: String): (String, Int) = l match
    case s"${k}: $v" => k -> v.toInt
  def parseGate(l: String): Gate = l match
    case s"$a $t $b -> $o" => Gate(a, b, t, o)
    
  val input: List[String]      = Source.fromResource("day24.txt").getLines.toList
  val wiring: Map[String, Int] = input.takeWhile(!_.isBlank).map(parseInput).toMap
  val gates: List[Gate]        = input.drop(wiring.size).tail.map(parseGate)

  @tailrec
  def circuit(wires: Map[String, Int], gates: List[Gate]): Map[String, Int] =
    def evaluate(wires: Map[String, Int], gate: Gate) =
      for a <- wires.get(gate.left); b <- wires.get(gate.right)
      yield gate.out ->  gate.eval(a, b)

    val (evaluated, remaining) =
      gates.partition(gate => wires.contains(gate.left) && wires.contains(gate.right))
    val input2 = evaluated.foldLeft(wires) { (currentValues, gate) =>
      evaluate(currentValues, gate) match
        case Some((outputWire, value)) => currentValues + (outputWire -> value)
        case None                      => currentValues
    }
    if remaining.isEmpty then input2 else circuit(input2, remaining)

  
  def partOne(): Long =
    val zs = circuit(wiring, gates).filter(_._1.head == 'z').values.toSeq.sorted
    bin2dec(zs.reverse.mkString)

  //def partTwo(): Int = 0
