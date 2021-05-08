import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec

class TestKSAlgorithm extends AnyFlatSpec {
  "A test" should "fail" in {
    assertThrows[TestFailedException] {
      fail()
    }
  }
  // TODO test whitenoise and update
}