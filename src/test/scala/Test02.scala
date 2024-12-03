import org.scalatest._
import assignments.Day02

class Test02 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day02.partOne() should be(2)
    Day02.partTwo() should be(4)
  }
