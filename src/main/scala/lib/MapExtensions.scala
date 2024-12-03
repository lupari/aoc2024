package lib

object MapExtensions:

  extension [A, B](map: Map[A, Seq[B]])
    def addOrUpdate(other: Map[A, B]): Map[A, Seq[B]] =
      val (existing, absent) = other.partition(o => map.contains(o._1))
      val map2               = map ++ absent.map(kv => kv._1 -> Seq(kv._2))
      existing.foldLeft(map2)((acc, xs) => acc + (xs._1 -> (acc(xs._1) :+ xs._2)))

  extension [A, B: Numeric](map: Map[A, B])
    def addOrUpdate(key: A, value: B): Map[A, B] =
      if map.contains(key) then map + (key -> (summon[Numeric[B]].plus(map(key), value)))
      else map + (key                      -> value)
