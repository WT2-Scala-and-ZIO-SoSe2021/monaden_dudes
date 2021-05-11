package exercise2

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

trait StackLike[T] {
  def top(): Option[T] = this match {
    case StackElement(head, _) => Some(head)
    case _ => None
  }

  def pop(): Try[StackLike[T]] = this match {
    case StackElement(_, tail) => Success(tail)
    case _ => Failure(new RuntimeException("Stack is empty!"))
  }

  def isEmpty: Boolean = this match {
    case StackElement(_, _) => false
    case _ => true
  }

  def reverse(): StackLike[T] = {
    @tailrec
    def doReverse(currEle: StackLike[T], tmpStack: StackLike[T]): StackLike[T] = currEle match {
      case StackElement(head, tail) => doReverse(tail, tmpStack.push(head))
      case _ => tmpStack
    }

    this match {
      case StackElement(head, tail) => doReverse(tail, StackElement[T](head))
      case _ => this
    }
  }

  def push(elem: T): StackLike[T] = StackElement[T](elem, this)
}

case class StackEndElement[T]() extends StackLike[T]

case class StackElement[T](head: T, tail: StackLike[T] = StackEndElement[T]()) extends StackLike[T]

trait QueueLike[T] {
  def enqueue(elem: T): QueueLike[T]

  def dequeue(): Try[QueueLike[T]]

  def front(): Option[T]

  def isEmpty: Boolean
}

case class DoubleStackQueue[T]
(in: StackLike[T] = StackEndElement[T](),
 out: StackLike[T] = StackEndElement[T]())
  extends QueueLike[T] {

  override def enqueue(elem: T): QueueLike[T] = DoubleStackQueue[T](in = in.push(elem))

  private def inToOutStack(): DoubleStackQueue[T] = DoubleStackQueue[T](out = in.reverse)

  override def dequeue(): Try[QueueLike[T]] = isEmpty match {
    case true => Failure(new RuntimeException("Queue is empty!"))
    case false => out.isEmpty match {
      case true => inToOutStack().dequeue()
      case false => Success(DoubleStackQueue[T](in = in, out = out.pop().get))
    }
  }

  override def front(): Option[T] = isEmpty match {
    case true => None
    case false => out.isEmpty match {
      case true => inToOutStack().front()
      case false => out.top()
    }
  }

  override def isEmpty: Boolean = in.isEmpty && out.isEmpty
}
