package lib

object NumberExtensions:
  
  extension (n: Long)
      def divIfEven(d: Long): Option[Long] = if (n % d == 0) Some(n / d) else None


