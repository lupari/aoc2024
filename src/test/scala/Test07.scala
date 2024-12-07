import assignments.Day07
import org.scalatest.*

class Test07 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day07.partOne() should be(1582598718861L)
    Day07.partTwo() should be(165278151522644L)
  }
