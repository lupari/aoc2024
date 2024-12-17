import assignments.Day17
import org.scalatest.*

class Test17 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day17.partOne() should be("1,5,0,1,7,4,1,0,3")
    Day17.partTwo() should be(47910079998866L)
  }
