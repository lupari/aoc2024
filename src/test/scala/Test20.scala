import assignments.Day20
import org.scalatest.*

class Test20 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day20.partOne() should be(1387)
    Day20.partTwo() should be(1015092)
  }
