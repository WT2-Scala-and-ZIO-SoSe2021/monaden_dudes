import org.scalatest.flatspec.AnyFlatSpec

class TestTaskTwoTypes extends AnyFlatSpec {
  // Ior Tests
  "Mapping a function on an Ior with a right value" should "result in new Right Ior with the new value" in {
    val ior = Ior.right(2)
    assert(ior.map(x => x * 4) == Right(8))
  }

  "Mapping a function on an Ior with both right value and left value" should "result in a new Both Ior with the new value" in {
    val exception = new RuntimeException("Error")
    val ior = Ior.both(exception, 2)
    val ior2 = ior.map(x => x * 4)
    assert(ior2 == Both(exception, 8))
  }

  // Stack Tests
  "A push on a stack" should "add the new element to the top of the stack" in {
    val stack = StackElem(1, StackElem(2, StackEmpty()))
    val pushStack = StackElem(10, StackElem(1, StackElem(2, StackEmpty())))
    assert(stack.push(10) == pushStack)
  }

  "A push on an empty stack" should "return a stack with one element" in {
    val stack = StackEmpty[Int]()
    val pushStack = StackElem(1, StackEmpty())
    assert(stack.push(1) == pushStack )
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
    val stackReversed = stack.reverse(None)
    val reverseStack = StackElem(3, StackElem(2, StackElem(1, StackEmpty())))
    assert(stackReversed.top.get == 3)
    assert(stackReversed.pop.get.top.get == 2)
    assert(stackReversed.pop.get.pop.get.top.get == 1)

    assert(stackReversed == reverseStack)
  }

  "A stack reverse on an empty stack" should "return the stack" in {
    val emptyStack = StackEmpty()
    assert(emptyStack.reverse(None) == StackEmpty())
  }
}
