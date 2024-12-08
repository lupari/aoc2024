import assignments.Day08
import org.scalatest.*

class Test08 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day08.partOne() should be(329)
    Day08.partTwo() should be(1190)
  }
