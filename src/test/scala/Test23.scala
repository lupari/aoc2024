import assignments.Day23
import org.scalatest.*

class Test23 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day23.partOne() should be(1077)
    Day23.partTwo() should be("bc,bf,do,dw,dx,ll,ol,qd,sc,ua,xc,yu,zt")
  }
