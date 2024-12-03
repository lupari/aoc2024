package lib

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.mutable.PriorityQueue

object Graphs:
  object dfs:
    def apply[A](start: A)(nf: A => Iterable[A]): Iterable[A] =
      def _dfs(s: A, seen: Iterable[A]): Iterable[A] =
        if seen.iterator.contains(s) then seen
        else
          val neighbors = nf(s).filterNot(seen.iterator.contains)
          neighbors.foldLeft(Iterable(s) ++ seen)((b, a) => _dfs(a, b))

      _dfs(start, Nil)

  object bfs:
    def traverse[A](start: A)(nf: A => Iterable[A]): Map[A, Int] =
      @tailrec
      def _traverse(seen: Map[A, Int], unseen: Map[A, Int]): Map[A, Int] =
        val neighbors = for {
          (node, cost) <- unseen; newNode <- nf(node)
        } yield newNode -> (cost + 1)
        val seen2   = seen ++ unseen
        val unseen2 = neighbors.filterNot(n => seen.contains(n._1))
        if unseen2.isEmpty then seen2 else _traverse(seen2, unseen2)

      _traverse(Map.empty, Map(start -> 0))

    def search[A](start: A)(nf: A => Iterable[A])(ef: A => Boolean): Option[Int] =
      @tailrec
      def _search(unseen: Iterable[A], cost: Map[A, Int]): Option[Int] = unseen match
        case h :: t if ef(h) => Some(cost(h))
        case h :: t =>
          val neighbors = nf(h).filterNot(cost.contains)
          _search(t ++ neighbors, cost ++ neighbors.map(n => (n -> (cost(h) + 1))))
        case _ => None

      _search(List(start), Map(start -> 0))

  object aStar:
    def apply[A](start: A, goal: A)(cf: (A, A) => Int)(nf: A => Iterable[A])(
        hf: A => Int
    ): (Map[A, Int], Option[(A, Int)]) =
      val seen: mutable.Map[A, Int]            = mutable.Map.empty
      val unseen: PriorityQueue[(Int, Int, A)] = PriorityQueue.empty(Ordering.by(-_._1))
      unseen.enqueue((hf(start), 0, start))
      while unseen.nonEmpty do
        val (_, dist, node) = unseen.dequeue()
        if !seen.contains(node) then
          seen(node) = dist
          if node == goal then return (seen.toMap, Some(node -> dist))
          else
            def visit(n: A, d: Int) =
              if !seen.contains(n) then unseen.enqueue((dist + d + hf(n), dist + d, n))
            nf(node).map(n => (n, cf(node, n))).foreach(n => visit(n._1, n._2))

      (seen.toMap, None)

  object dijkstra:
    def apply[A](start: A, goal: A)(nf: A => Iterable[A])(
        cf: (A, A) => Int
    ): (Map[A, Int], Option[(A, Int)]) =
      val seen: mutable.Map[A, Int]       = mutable.Map.empty
      val unseen: PriorityQueue[(Int, A)] = PriorityQueue.empty(Ordering.by(-_._1))
      unseen.enqueue((0, start))
      while unseen.nonEmpty do
        val (dist, node) = unseen.dequeue()
        if !seen.contains(node) then
          seen(node) = dist
          if node == goal then return (seen.toMap, Some(node -> dist))
          else
            def visit(n: A, d: Int) =
              if !seen.contains(n) then unseen.enqueue((dist + d, n))
            nf(node).map(n => (n, cf(node, n))).foreach(n => visit(n._1, n._2))

      (seen.toMap, None)

  object floodfill:
    def apply[A](start: A, nf: A => Set[A])(ff: A => Boolean): Set[A] =
      @tailrec
      def helper(visited: Set[A], open: Queue[A]): Set[A] =
        open.dequeueOption match
          case Some((current, open)) =>
            val neighbors  = nf(current).filter(ff) -- visited - current
            val newVisited = visited ++ neighbors
            val newOpen    = open.enqueueAll(neighbors)
            helper(newVisited, newOpen)
          case None => visited
      helper(Set(start), Queue(start))
