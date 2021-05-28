package exercise3
import temperature._
import zio.console._

object temperatureTask {

  def f(x: Temperature)(implicit f1:Temperature=>Temperature) = ???

  def main(args: Array[String]): Unit = {
    val t1: Temperature = 2d
    val t2: Temperature = 3d

//    println(t1.celsius avg t2.celsius)
    println(display(freezingPoint)(Other))
    println(display(freezingPoint)(US))
    println(display(freezingPoint)(SCI))
    println(display(absoluteZero)(Other))
    println(display(absoluteZero)(US))
    println(display(absoluteZero)(SCI))

    implicit val locale: Locale = SCI

//    println(display(t1))
//    println(display(t1)(US))
//    println(display(t1)(Other))
  }
}