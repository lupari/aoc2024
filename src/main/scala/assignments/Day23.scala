package assignments

import scala.io.Source
import lib.Graphs.bronKerbosch

object Day23:
  import lib.IteratorExtensions.*
  def parse(l: String): Set[(String, String)] =
    val parts = l.split("-").unpack
    Set(parts, parts.swap)
  val party: Set[(String, String)] = Source.fromResource("day23.txt").getLines.flatMap(parse).toSet
  val adjacency: Map[String, Set[String]] = party.groupMap(_._1)(_._2)

  def partOne(): Int =
    val triplets = for
      (node, neighbors) <- adjacency; n1 <- neighbors; n2 <- neighbors if adjacency(n1).contains(n2)
    yield List(node, n1, n2).sorted
    triplets.toList.distinct.count(_.exists(_.startsWith("t")))

  def partTwo(): String = bronKerbosch(adjacency).maxBy(_.size).toSeq.sorted.mkString(",")
