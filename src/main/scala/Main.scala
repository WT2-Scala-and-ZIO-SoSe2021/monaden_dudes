object Main {

  def arrayToString[X](x: X): String = x match {
    case arr: Array[_] => arr.map(arrayToString).mkString("[", ", ", "]")
    case _ => x.toString
  }

  def max(arr: Array[Int]): Int = arr.reduce((a, b) => if (a > b) a else b)

  def min(arr: Array[Int]): Int = arr.reduce((a, b) => if (a < b) a else b)

  def sum(arr: Array[Int]): Int = arr.reduce((a, b) => a + b)

  def parse(cardName: String): Int = cardName match {
    case "2" => 2
    case "3" => 3
    case "4" => 4
    case "5" => 5
    case "6" => 6
    case "7" => 7
    case "8" => 8
    case "9" => 9
    case "J" | "Q" | "K" => 10
    case "A" => 11
  }

  def parseAll(cardNames: Array[String]): Array[Int] = cardNames.map(c => parse(c))

  def values(card: Int): Array[Int] = card match {
    case 11 => Array(1, 11)
    case _ => Array(card)
  }

  def main(args: Array[String]): Unit = {
    println(arrayToString(parse("2")))
    println(arrayToString(parseAll(Array("2", "A"))))
    println(arrayToString(values(11)))
    println(arrayToString(values(1)))
  }
}
