import assignments.Day22
import org.scalatest.*

class Test22 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day22.partOne() should be(17262627539L)
    Day22.partTwo() should be(1986)
  }
