package lib

import scala.annotation.tailrec

object Search:
  object binSearch:
    def apply[A, B](xs: Iterable[A])(decide: Iterable[A] => Option[B]): (Iterable[A], Iterable[A]) =
      @tailrec
      def helper(lower: Int, upper: Int): (Iterable[A], Iterable[A]) =
        if lower >= upper then xs.splitAt(lower)
        else
          val mid = (lower + upper) / 2
          decide(xs.take(mid)) match
            case None => helper(lower, mid)
            case _    => helper(mid + 1, upper)

      helper(0, xs.size)

