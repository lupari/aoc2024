import assignments.Day19
import org.scalatest.*

class Test19 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day19.partOne() should be(360)
    Day19.partTwo() should be(577474410989846L)
  }
