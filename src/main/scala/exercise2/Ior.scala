package exercise2

sealed trait Ior[E, A] {
  // todo: independent error type? => E_2?
  def flatMap[B](f: A => Ior[E, B]): Ior[E, B] = this match {
    case Left(error) => Ior.left[E, B](error)
    case Right(element) => f(element)
    case Both(error, element) => f(element) match {
      case l@Left(_) => l
      case Right(element) => Ior.both(error, element)
      case b@Both(_, _) => b
    }
  }

  def map[B](f: A => B): Ior[E, B] = flatMap(x => Ior.unit(f(x)))
}

object Ior {
  def unit[E, A](element: A): Ior[E, A] = Right[E, A](element)
  def left[E, A](error: E): Left[E, A] = Left[E, A](error)
  def right[E, A](element: A): Right[E, A] = Right[E, A](element)
  def both[E, A](error: E, element: A): Both[E, A] = Both[E, A](error, element)
}

case class Left[E, A](error: E) extends Ior[E, A]
case class Right[E, A](element: A) extends Ior[E, A]
case class Both[E, A](error: E, element: A) extends Ior[E, A]


