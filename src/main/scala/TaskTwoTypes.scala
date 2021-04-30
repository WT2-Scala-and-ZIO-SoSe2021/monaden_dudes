sealed trait Ior[A] {
//  def flatMap[A, B](f: A => Ior[B]): Ior[B] = {
//
//  }
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
  def unit[A](elem: A):Ior[A] = Ior[A]
}



