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