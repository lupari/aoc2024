package assignments

import scala.annotation.tailrec
import scala.io.Source
import lib.Numbers.bin2dec

object Day24:
  case class Gate(a: String, op: String, b: String, out: String):
    def eval(i1: Int, i2: Int): Int = op match
      case "AND" => i1 & i2
      case "OR"  => i1 | i2
      case "XOR" => i1 ^ i2
    // outputs to Z but does not do XOR
    def isOutputFault: Boolean = out.head == 'z' && out != "z45" && op != "XOR"
    // neither takes input nor outputs to Z but does XOR
    def isInternalFault: Boolean = a.head < 'x' && b.head < 'x' && out.head != 'z' && op == "XOR"

  def parseWire(l: String): (String, Int) = l match
    case s"${k}: $v" => k -> v.toInt
  def parseGate(l: String): Gate = l match
    case s"$a $op $b -> $out" => Gate(a, op, b, out)

  def run(wires: Map[String, Int], gates: List[Gate]): Long =
    @tailrec
    def helper(xs: List[Gate], acc: Map[String, Int]): Map[String, Int] =
      if xs.isEmpty then acc
      else
        val (seen, unseen) = xs.partition(g => acc.contains(g.a) && acc.contains(g.b))
        val acc2           = seen.foldLeft(acc)((w, g) => w + (g.out -> g.eval(w(g.a), w(g.b))))
        helper(unseen, acc2)

    asLong(helper(gates, wires), 'z')

  def asLong(wires: Map[String, Int], c: Char): Long =
    bin2dec(wires.filter(_._1.head == c).toSeq.sorted.map(_._2).reverse.mkString)

  val input: List[String]     = Source.fromResource("day24.txt").getLines.toList
  val wires: Map[String, Int] = input.takeWhile(!_.isBlank).map(parseWire).toMap
  val gates: List[Gate]       = input.drop(wires.size).tail.map(parseGate)

  def partOne(): Long = run(wires, gates)
  def partTwo(): String =

    def swap(gs1: List[Gate], gs2: List[Gate]): List[Gate] =
      val updated = gs2.map(gate => gate.out -> gate).toMap
      gs1.map(gate => updated.getOrElse(gate.out, gate))

    def findZ(gs: List[Gate], out: String): Option[String] =
      val in = gs.filter(gate => gate.a == out || gate.b == out)
      in.find(_.out.head == 'z') match
        case Some(gate) => Some("z%02d".format(gate.out.tail.toInt - 1))
        case None       => in.map(g => findZ(gates, g.out)).collectFirst { case Some(z) => z }

    @tailrec
    def pair(ifs: List[Gate], accOut: List[Gate], accInt: List[Gate]): List[Gate] = ifs match
      case fault :: t =>
        val (in, i)       = accOut.zipWithIndex.find(_._1.out == findZ(gates, fault.out).get).get
        val (fault2, in2) = (fault.copy(out = in.out), in.copy(out = fault.out))
        pair(t, accOut.updated(i, in2), accInt :+ fault2)
      case _ => accOut ++ accInt

    val (out, int) = (gates.filter(_.isOutputFault), gates.filter(_.isInternalFault))
    val faults     = pair(int, out, Nil)
    val Seq(x, y)  = Seq((wires, 'x'), (wires, 'y')).map(asLong)
    val z          = run(wires, swap(gates, faults))
    val lead       = (x + y ^ z).toBinaryString.dropWhile(_ == '1').length.toString
    val allFaults  = faults ++ gates.filter(g => g.a.endsWith(lead) && g.b.endsWith(lead))
    allFaults.map(_.out).sorted.mkString(",")
