package lib

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.mutable.PriorityQueue

object Graphs:
  object dfs:
    // provides all nodes accessible from start, does not keep track of paths
    def apply[A](start: A)(nf: A => Iterable[A]): Iterable[A] =
      def _dfs(s: A, seen: Iterable[A]): Iterable[A] =
        if seen.iterator.contains(s) then seen
        else
          val neighbors = nf(s).filterNot(seen.iterator.contains)
          neighbors.foldLeft(Iterable(s) ++ seen)((b, a) => _dfs(a, b))

      _dfs(start, Nil)

    // provides all accessible paths to an end node from start
    def search[A](start: A)(next: A => Seq[A])(goal: A => Boolean): Seq[Seq[A]] =
      def _dfs(p: A, path: Seq[A], visited: Set[A]): Seq[Seq[A]] =
        if goal(p) then Seq(path)
        else
          val neighbors = next(p).filterNot(visited.contains)
          neighbors.flatMap(n => _dfs(n, path :+ n, visited + n))

      _dfs(start, Seq(start), Set(start))

  object bfs:
    def traverse[A](start: A)(nf: A => Iterable[A]): Map[A, Int] =
      @tailrec
      def _traverse(seen: Map[A, Int], unseen: Map[A, Int]): Map[A, Int] =
        val neighbors = for (node, cost) <- unseen; newNode <- nf(node) yield newNode -> (cost + 1)
        val seen2     = seen ++ unseen
        val unseen2   = neighbors.filterNot(n => seen.contains(n._1))
        if unseen2.isEmpty then seen2 else _traverse(seen2, unseen2)

      _traverse(Map.empty, Map(start -> 0))

    def search[A](start: A)(nf: A => Iterable[A])(ef: A => Boolean): Option[(Int, List[A])] =
      @tailrec
      def _search(
          unseen: Iterable[A],
          cost: Map[A, Int],
          predecessors: Map[A, A]
      ): Option[(Int, List[A])] = unseen match
        case h :: t if ef(h) =>
          // Reconstruct the path by backtracking from the end node
          val path = Iterator
            .iterate(h)(predecessors)
            .takeWhile(predecessors.contains)
            .toList
            .reverse :+ h
          Some(cost(h), start +: path)
        case h :: t =>
          val neighbors = nf(h).filterNot(cost.contains)
          _search(
            t ++ neighbors,
            cost ++ neighbors.map(n => n -> (cost(h) + 1)),
            predecessors ++ neighbors.map(n => n -> h)
          )
        case _ => None

      _search(List(start), Map(start -> 0), Map.empty)

  object aStar:
    def apply[A](start: A, goal: A)(nf: A => Set[(A, Int)])(hf: A => Long): Option[Long] =
      case class Node(point: A, cost: Long, estimatedTotalCost: Long)
      val priorityQueue = mutable.PriorityQueue.empty[Node](Ordering.by(-_.estimatedTotalCost))
      priorityQueue.enqueue(Node(start, 0, hf(start)))

      val visited   = collection.mutable.Set.empty[A]
      val bestCosts = mutable.Map[A, Long](start -> 0)

      while priorityQueue.nonEmpty do
        val current = priorityQueue.dequeue()
        if current.point == goal then return Some(current.cost)
        nf(current.point).foreach { case (neighbor, moveCost) =>
          val newCost = current.cost + moveCost
          if newCost < bestCosts.getOrElse(neighbor, Long.MaxValue) then
            bestCosts(neighbor) = newCost
            val estimatedTotalCost = newCost + hf(neighbor)
            priorityQueue.enqueue(Node(neighbor, newCost, estimatedTotalCost))
        }
      None

  object dijkstra:
    def apply[A](start: A)(nf: A => Set[(A, Int)])(
        ef: A => Boolean
    ): (Map[A, Int], Option[(A, Int)]) =
      val distances = mutable.Map[A, Int](start -> 0)
      val unseen    = mutable.PriorityQueue((0, start))(Ordering.by(-_._1))
      val visited   = mutable.Set.empty[A]
      while unseen.nonEmpty do
        val (currentDist, currentNode) = unseen.dequeue()
        if !visited.contains(currentNode) then
          visited.add(currentNode)
          if ef(currentNode) then return (distances.toMap, Some(currentNode -> currentDist))
          for (neighbor, weight) <- nf(currentNode) do
            val newDist = currentDist + weight
            if newDist < distances.getOrElse(neighbor, Int.MaxValue) then
              distances(neighbor) = newDist
              unseen.enqueue((newDist, neighbor))

      (distances.toMap, None)

  object floodfill:
    def apply[A](start: A, nf: A => Iterable[A])(ff: A => Boolean): Set[A] =
      if !ff(start) then Set.empty
      else
        @tailrec
        def helper(visited: Set[A], open: Queue[A]): Set[A] =
          open.dequeueOption match
            case Some((current, open)) =>
              val neighbors  = nf(current).filter(ff).toSet -- visited
              val newVisited = visited ++ neighbors
              val newOpen    = open.enqueueAll(neighbors)
              helper(newVisited, newOpen)
            case None => visited
        helper(Set(start), Queue(start))

  object tsort:
    def apply[A](nodes: Iterable[A], graph: Map[A, Iterable[A]]): List[A] =
      def visit(node: A, visited: Set[A], sorted: List[A]): (Set[A], List[A]) =
        if visited.contains(node) then (visited, sorted)
        else
          val dependencies = graph.getOrElse(node, Nil)
          val (newVisited, newSorted) = dependencies.foldLeft((visited + node, sorted)) {
            case ((vis, sort), dep) => visit(dep, vis, sort)
          }
          (newVisited, node :: newSorted)
      nodes
        .foldLeft((Set.empty[A], List.empty[A])) { case ((visited, sorted), node) =>
          visit(node, visited, sorted)
        }
        ._2
        .reverse

  object bronKerbosch:
    def apply[A](adjacency: Map[A, Set[A]]): Set[Set[A]] =

      // Select a pivot from the union of P and X to minimize branching
      def selectPivot(P: Set[A], X: Set[A]): A =
        // Find the vertex in P ∪ X that has the most neighbors in P
        (P ++ X).maxBy(v => adjacency.getOrElse(v, Set()).intersect(P).size)

      def helper(R: Set[A], P: Set[A], X: Set[A], cliques: Set[Set[A]]): Set[Set[A]] =
        // If P and X are both empty, R is a maximal clique
        if P.isEmpty && X.isEmpty then cliques + R
        else
          // Select a pivot (to minimize branching)
          val pivot = selectPivot(P, X)

          // Iterate over the set of potential candidates P \ {pivot's neighbors}
          val newP = P -- adjacency(pivot)
          newP.foldLeft(cliques) { (cliquesAcc, v) =>
            // Recurse with updated sets
            val newR = R + v
            val newP = P.intersect(adjacency(v))
            val newX = X.intersect(adjacency(v))

            // Recurse with the updated sets and add new cliques
            helper(newR, newP, newX, cliquesAcc) ++ cliquesAcc
          }

      helper(Set(), adjacency.keySet, Set(), Set())
