package exercise2

import org.scalatest.flatspec.AnyFlatSpec

class TestSequentialCollections extends AnyFlatSpec {
  "A push on a stack" should "add the new element to the top of the stack" in {
    val stack = StackElement(1, StackElement(2))
    val resultStack = StackElement(10, StackElement(1, StackElement(2)))
    assert(stack.push(10) == resultStack)
  }

  "A push on a stack (syntax sugar)" should "add the new element to the top of the stack" in {
    val stack = StackLike(1,2)
    val resultStack = StackLike(10,1,2)
    assert(stack.push(10) == resultStack)
  }


  "A push on an empty stack" should "return a stack with one element" in {
    val stack = StackEndElement[Int]()
    val resultStack = StackElement(1)
    assert(stack.push(1) == resultStack)
  }

  "A pop on a stack with 3 elements" should "return a new stack with two elements" in {
    val stack = StackElement(1, StackElement(2, StackElement(3)))
    val poppedStack = stack.pop()
    assert(poppedStack.get.top().get == 2)
  }

  "A pop on a stack with 1 element" should "return an empty stack" in {
    val stack = StackElement(1)
    val popTry = stack.pop()
    assert(popTry.isSuccess)
    assert(popTry.get.isEmpty)
  }

  "A pop on an empty Stack" should "raise an exception" in {
    val stack = StackEndElement[Int]()
    assert(stack.pop().isFailure)
  }

  "The top of a stack" should "return the first element in the stack" in {
    val stack = StackElement(1, StackElement(2, StackElement(3)))
    assert(stack.top().get == 1)
  }

  "An isEmpty call on a stack with Elements" should "return false" in {
    val stack = StackElement(1)
    assert(!stack.isEmpty)
  }

  "An isEmpty call on a stack without Elements" should "return true" in {
    val stack = StackEndElement[Int]()
    assert(stack.isEmpty)
  }

  "A reverse of a stack with multiple values" should "return a new stack with the order of elements reversed" in {
    val stack = StackElement(1, StackElement(2, StackElement(3)))
    val reversedStack = stack.reverse
    val trueReversedStack = StackElement(3, StackElement(2, StackElement(1)))
    assert(reversedStack.top.get == 3)
    assert(reversedStack.pop.get.top.get == 2)
    assert(reversedStack.pop.get.pop.get.top.get == 1)

    assert(reversedStack == trueReversedStack)
  }

  "A reverse of a stack with one value" should "return the same stack" in {
    val stack = StackElement(1)
    val reversedStack = stack.reverse
    val trueReversedStack = StackElement(1)

    assert(reversedStack.top.get == 1)
    assert(reversedStack == trueReversedStack)
  }

  "A stack reverse on an empty stack" should "return the stack" in {
    val emptyStack = StackEndElement[Int]()
    assert(emptyStack.reverse == StackEndElement[Int]())
  }

  "Getting the front element of a non-empty queue" should "return the first element of the outqueue / last element of the inqueue" in {
    val queue1 = DoubleStackQueue(StackElement(1, StackElement(2)))
    val queue2 = DoubleStackQueue(out = StackElement(2, StackElement(1)))
    assert(queue1.front() == Option(2))
    assert(queue2.front() == Option(2))
  }

  "An enqueue on an empty queue" should "return a non-empty queue" in {
    val queue = DoubleStackQueue[Int]()
    val queue1 = queue.enqueue(1)
    assert(queue.isEmpty)
    assert(!queue1.isEmpty)
    assert(queue1.front() == Option(1))
  }

  "Dequeuing on a queue with non-empty outqueue" should "remove the first element of the outqueue" in {
    val queue = DoubleStackQueue(out = StackElement(1))
    val queue1 = queue.dequeue()
    val queue2 = DoubleStackQueue(out = StackElement(2, StackElement(1)))
    val queue3 = queue2.dequeue()
    assert(queue1.get.isEmpty)
    assert(queue2.front() == Option(2))
    assert(queue3.get.front() == Option(1))
  }
}
