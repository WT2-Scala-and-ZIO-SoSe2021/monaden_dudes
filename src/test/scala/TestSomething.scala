import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec

class TestSomething extends AnyFlatSpec {
  "A test" should "fail" in {
    assertThrows[TestFailedException] {
      fail()
    }
  }
}
