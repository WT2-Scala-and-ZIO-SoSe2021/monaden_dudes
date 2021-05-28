package object temperature {
  type Temperature = Double

  val freezingPoint:Temperature = 0.0.celsius
  val absoluteZero:Temperature = -273.15.celsius

  trait Locale
  case object US extends Locale
  case object SCI extends Locale
  case object Other extends Locale

  implicit val locale: Locale = Other

  def display(temp: Temperature)(implicit locale: Locale): String = {
    locale match {
      case US => s"${temp.fahrenheit} °F"
      case SCI =>s"${temp.kelvin} °K"
      case Other => s"$temp °C"
    }
  }

  implicit class TemperatureSyntax(t: Double){

    def fahrenheit: Temperature = {
      (t* (5.0/9.0)) + 32
    }
    def kelvin: Temperature = {
      t + 273.15
    }
    def celsius: Temperature = t

    def avg(t2: Temperature): Temperature = (t + t2) / 2
  }
}
