package exercise3
import temperature._

object temperatureTask {

  def f(x: Temperature)(implicit f1:Temperature=>Temperature) = ???

  def main(args: Array[String]): Unit = {
    val t1: Temperature = 2d
    val t2: Temperature = 3d

    println(display(freezingPoint)(Other))
    println(display(freezingPoint)(US))
    println(display(freezingPoint)(SCI))
    println(display(absoluteZero)(Other))
    println(display(absoluteZero)(US))
    println(display(absoluteZero)(SCI))

    println("setting locale to Scientific Mode, temperature should display as Kelvin")
    implicit val locale: Locale = SCI
    println(display(2.0))
  }
}