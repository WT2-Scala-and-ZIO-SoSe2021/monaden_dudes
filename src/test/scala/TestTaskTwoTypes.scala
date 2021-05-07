import exercise2.{Both, Ior, Left, QueueImpl, Right, StackElem, StackEmpty}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Success

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

  "A stack pop on an empty Stack" should "return an empty stack" in {
    val stack = StackEmpty()
    assert(stack.pop() == Success(StackEmpty()))
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

  // Queue Tests
  "Getting the front element of a non-empty queue" should "return the first element of the outqueue / last element of the inqueue" in {
    val queue1 = QueueImpl(StackElem(1, StackElem(2, StackEmpty())), StackEmpty())
    val queue2 = QueueImpl(StackEmpty(), StackElem(2, StackElem(1, StackEmpty())))
    assert(queue1.front() == Option(2))
    assert(queue2.front() == Option(2))
  }

  "An enqueue on an empty queue" should "return a non-empty queue" in {
    val queue = QueueImpl(StackEmpty[Int](), StackEmpty[Int]())
    val queue1 = queue.enqueue(1)
    assert(queue.isEmpty)
    assert(!queue1.isEmpty)
    assert(queue1.front() == Option(1))
  }

  "Dequeuing on a queue with non-empty outqueue" should "remove the first element of the outqueue" in {
    val queue = QueueImpl(StackEmpty(), StackElem(1, StackEmpty()))
    val queue1 = queue.dequeue()
    val queue2 = QueueImpl(StackEmpty(), StackElem(2, StackElem(1, StackEmpty())))
    val queue3 = queue2.dequeue()
    assert(queue1.get.isEmpty)
    assert(queue2.front() == Option(2))
    assert(queue3.get.front() == Option(1))
  }
}
