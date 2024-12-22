import assignments.Day21
import org.scalatest.*

class Test21 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day21.partOne() should be(109758)
    Day21.partTwo() should be(134341709499296L)
  }
