package lib

import lib.Points.Point

object IteratorExtensions:

  extension [A](xs: Iterable[A])
    def unpack: (A, A)           = (xs.head, xs.last)
    def allPairs: Seq[(A, A)]    = xs.toSeq.combinations(2).toSeq.map(p => (p.head, p.last))
    def groupCount: Map[A, Long] = xs.groupMapReduce(identity)(_ => 1L)(_ + _)
