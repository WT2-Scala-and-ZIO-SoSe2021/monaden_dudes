import exercise1.TaskOneOperations
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestTaskOneOps extends AnyFlatSpec with should.Matchers {

  "An empty Array" should "throw an exception on min" in {
    assertThrows[UnsupportedOperationException] {
      TaskOneOperations.min(Array())
    }
  }
  it should "throw an exception on max" in {
    assertThrows[UnsupportedOperationException] {
      TaskOneOperations.max(Array())
    }
  }
  it should "throw an exception on sum" in {
    assertThrows[UnsupportedOperationException] {
      TaskOneOperations.sum(Array())
    }
  }

  "An Array with the values: 1" should "have a min of 1" in {
    TaskOneOperations.min(Array(1)) should be (1)
  }
  it should "have a max of 1" in {
    TaskOneOperations.max(Array(1)) should be (1)
  }
  it should "have a sum of 1" in {
    TaskOneOperations.sum(Array(1)) should be (1)
  }

  "An Array with the values: 1, 2, 3, -4" should "have a min of -4" in {
    TaskOneOperations.min(Array(1, 2, 3, -4)) should be (-4)
  }
  it should "have a max of 3" in {
    TaskOneOperations.max(Array(1, 2, 3, -4)) should be (3)
  }
  it should "have a sum of 1" in {
    TaskOneOperations.sum(Array(1, 2, 3, -4)) should be (2)
  }
}
