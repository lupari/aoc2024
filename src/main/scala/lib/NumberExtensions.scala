package lib

import scala.annotation.targetName
import scala.math.Integral.Implicits._

object NumberExtensions:

  extension [A: Integral](n: A)
    @targetName("divMod") def /%(d: A): Option[A] = if (n % d == 0) Some(n / d) else None
    @targetName("safeAdd") def %+(d: A): A        = (n % d + d) % d
