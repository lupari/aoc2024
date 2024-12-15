import assignments.Day15
import org.scalatest.*

class Test15 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day15.partOne() should be(1398947)
    Day15.partTwo() should be(1397393)
  }
