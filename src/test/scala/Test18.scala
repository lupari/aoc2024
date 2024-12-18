import assignments.Day18
import org.scalatest.*

class Test18 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day18.partOne() should be(360)
    Day18.partTwo() should be("58,62")
  }
