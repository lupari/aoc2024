import assignments.Day16
import org.scalatest.*

class Test16 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day16.partOne() should be(134588)
    Day16.partTwo() should be(631)
  }
