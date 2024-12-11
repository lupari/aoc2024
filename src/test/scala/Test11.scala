import assignments.Day11
import org.scalatest.*

class Test11 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day11.partOne() should be(216996)
    Day11.partTwo() should be(257335372288947L)
  }
