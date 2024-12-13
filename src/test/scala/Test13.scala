import assignments.Day13
import org.scalatest.*

class Test13 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day13.partOne() should be(36571)
    Day13.partTwo() should be(85527711500010L)
  }
