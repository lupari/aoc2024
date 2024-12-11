import assignments.Day09
import org.scalatest.*

class Test09 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day09.partOne() should be(6370402949053L)
    //Day09.partTwo() should be(6398096697992L)
  }
