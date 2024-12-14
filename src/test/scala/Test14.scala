import assignments.Day14
import org.scalatest.*

class Test14 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day14.partOne() should be(226236192)
    Day14.partTwo() should be(8168)
  }
