import assignments.Day10
import org.scalatest.*

class Test10 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day10.partOne() should be(611)
    Day10.partTwo() should be(1380)
  }
