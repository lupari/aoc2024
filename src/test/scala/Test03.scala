import org.scalatest._
import assignments.Day03

class Test03 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day03.partOne() should be(182619815)
    Day03.partTwo() should be(80747545)
  }
