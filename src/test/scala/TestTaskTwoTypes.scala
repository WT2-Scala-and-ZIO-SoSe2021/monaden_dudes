import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec

class TestTaskTwoTypes extends AnyFlatSpec {
  //TODO push test
  "A push on a stack" should "add the new element to the top of the stack" in {
    val stack = StackElem(1, StackElem(2, StackEmpty()))
    val pushStack = StackElem(10, StackElem(1, StackElem(2, StackEmpty())))
    assert(stack.push(10) == pushStack)
  }

  "A stack pop with 3 elements" should "return a new stack with two elements" in {
    val stack = StackElem(1, StackElem(2, StackElem(3, StackEmpty())))
    val poppedStack = stack.pop
    assert(poppedStack.get.top.get == 2)
  }

  "The top of a stack" should "return the first element in the stack" in {
    val stack = StackElem(1, StackElem(2, StackElem(3, StackEmpty())))
    assert(stack.top.get == 1)
  }

  "An isEmpty call on a stack with Elements" should "return false" in {
    val stack = StackElem(1, StackEmpty())
    assert(!stack.isEmpty)
  }

  "An isEmpty call on a stack without Elements" should "return true" in {
    val stack = StackEmpty()
    assert(stack.isEmpty)
  }

  "A stack reverse" should "return a new stack with reverse order elements" in {
    val stack = StackElem(1, StackElem(2, StackElem(3, StackEmpty())))
    val stackReversed = stack.reverse
    val reverseStack = StackElem(3, StackElem(2, StackElem(1, StackEmpty())))
    //assert(stackReversed.top.get == 3)
    //assert(stackReversed.pop.get.top.get == 2)
    //assert(stackReversed.pop.get.pop.get.top.get == 1)

    assert(stackReversed == reverseStack)
  }

  "A stack reverse on an empty stack" should "return the stack" in {
    val emptyStack = StackEmpty()
    assert(emptyStack.reverse == StackEmpty())
  }
}
