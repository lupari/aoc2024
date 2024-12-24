import assignments.Day24
import org.scalatest.*

class Test24 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day24.partOne() should be(60714423975686L)
    //Day24.partTwo() should be(0)
  }
