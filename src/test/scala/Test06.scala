import org.scalatest._
import assignments.Day06

class Test06 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day06.partOne() should be(4665)
    Day06.partTwo() should be(1688)
  }
