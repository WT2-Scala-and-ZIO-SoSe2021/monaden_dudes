package exercise1

import exercise1.Operations.{max, min}

object TwentyOneGame {
  def arrayToString[X](x: X): String = x match {
    case arr: Array[_] => arr.map(arrayToString).mkString("[", ", ", "]")
    case _ => x.toString
  }

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

  def isBust(handValue: Int): Boolean = handValue match {
    case x if x > 21 => true
    case _ => false
  }

  // simplified method
  def determineHandValue(strategy: Array[Int] => Int)(hand: Array[Int]): Int =
    hand.map(x => strategy(values(x))).sum

  def optimisticF: Array[Int] => Int = determineHandValue(max)

  def pessimisticF: Array[Int] => Int = determineHandValue(min)


  def determineBestHandValue(hand: Array[Int]): Int = optimisticF(hand) match {
    case x if isBust(x) => pessimisticF(hand)
    case other => other
  }

  // context conditional Method
  def contextF(hand: Array[Int], eleven: Boolean, bust: Boolean): Int = if (bust) {
    pessimisticF(hand)
  } else {
    if (eleven) {
      pessimisticF(hand) + 10
    } else {
      optimisticF(hand)
    }
  }

  def determineBetterHandValue(hand: Array[Int]): Int = contextF(hand, eleven = false, bust = false) match {
    case x1 if isBust(x1) => contextF(hand, eleven = true, bust = false) match {
      case x2 if isBust(x2) => contextF(hand, eleven = true, bust = true)
      case other => other
    }
    case other => other
  }

  // credit to kylewlacy https://gist.github.com/kylewlacy/38681caaee2b98949dd8
  def partialCartesian(a: Array[Array[Int]], b: Array[Int]): Array[Array[Int]] = {
    a.flatMap(xs => {
      b.map(y => {
        xs ++ Array(y)
      })
    })
  }

  def cartesianProduct(lists: Array[Array[Int]]): Array[Array[Int]] = {
    lists.headOption match {
      case Some(head) => {
        val tail = lists.tail
        val init = head.map(n => Array(n))

        tail.foldLeft(init)((arr, list) => {
          partialCartesian(arr, list)
        })
      }
      case None => {
        Array()
      }
    }
  }

  def determineBestestHandValue(hand: Array[Int]): Int = {
    cartesianProduct(hand.map(values)).map(_.sum).filter(!isBust(_)).max
  }


  def main(args: Array[String]): Unit = {
    //    println(arrayToString(parse("2")))
    //    println(arrayToString(parseAll(Array("2", "A"))))
    //    println(arrayToString(values(11)))
    //    println(arrayToString(values(1)))
    val hand = Array(8, 11, 11)
    println(determineBestestHandValue(hand))
  }
}
