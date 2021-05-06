import scala.util.Try

sealed trait Ior[A] {
  def flatMap[B](f: A => Ior[B]): Ior[B] = this match {
    case Left(elem) => Ior.left(elem)
    case Right(elem) => f(elem)
    case Both(err, elem) => f(elem) match {
      case x@Left(_) => x
      case Right(elem) => Both(err, elem)
      case x@Both(_, _) => x
    }
  }

  def map[B](f: A => B):Ior[B] = flatMap(x => Ior.unit(f(x)))
}

case class Left[A](elem: Throwable) extends Ior[A]
case class Right[A](elem: A) extends Ior[A]
case class Both[A](left: Throwable, elem: A) extends Ior[A]

object Ior {
  def left[A](elem: Throwable): Left[A] = {
    Left(elem)
  }

  def right[A](elem: A): Right[A] = {
    Right(elem)
  }

  def both[A](left: Throwable, elem: A): Both[A] = {
    Both(left, elem)
  }

  def unit[A](elem: A): Ior[A] = right(elem)
}



trait StackLike[T] {
  def push(elem: T): StackLike[T] = StackElem(elem, this)

  def pop(): Try[StackLike[T]] = this match {
    case StackElem(elem, tail) => Try(tail)
    case StackEmpty() => Try(StackEmpty())
  }

  def top(): Option[T]

  def isEmpty: Boolean

  def reverse: StackLike[T]
}

case class StackElem[T](elem: T, tail: StackLike[T]) extends StackLike[T] {

  override def top(): Option[T] = Option(elem)

  override def isEmpty: Boolean = false

  override def reverse: StackLike[T] = {
//    tail match {
//      case StackElem(next_elem, next_tail) => {
//        val reversedStack = StackElem(elem, StackEmpty()).push(next_elem)
//        next_tail match {
//          case StackElem(_, _) => ???
//          case StackEmpty() => reversedStack
//        }
//      }
//      case StackEmpty() => StackElem(elem, StackEmpty())
//    }
    ???
  }
}

case class StackEmpty[T]() extends StackLike[T] {

  override def top(): Option[T] = None

  override def isEmpty: Boolean = true

  override def reverse: StackLike[T] = this
}

trait QueueLike[T] {
  def enqueue(elem: T): QueueLike[T]

  def dequeue(): Try[QueueLike[T]]

  def front(): Option[T]

  def isEmpty: Boolean
}