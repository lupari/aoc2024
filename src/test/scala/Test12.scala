import assignments.Day12
import org.scalatest.*

class Test12 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day12.partOne() should be(1431316)
    Day12.partTwo() should be(821428)
  }
    