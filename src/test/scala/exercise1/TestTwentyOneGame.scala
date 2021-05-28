package exercise1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestTwentyOneGame extends AnyFlatSpec with should.Matchers {
  "Low Hand value" should "not result in isBust true" in {
    assert(!TwentyOneGame.isBust(0))
  }
  "High Hand value" should "result in isBust true" in {
    assert(TwentyOneGame.isBust(99))
  }
  "Negative Hand value" should "not result in isBust true" in {
    assert(!TwentyOneGame.isBust(0))
  }
  "An empty Hand" should "result in Hand value 0" in {
    assert(TwentyOneGame.optimisticF(Array[Int]()) == 0)
    assert(TwentyOneGame.pessimisticF(Array[Int]()) == 0)
    assert(TwentyOneGame.determineBetterHandValue(Array[Int]()) == 0)
  }
  "A high standard value Hand" should "always result in its high value" in {
    val hand = Array(10, 10, 10)
    assert(TwentyOneGame.optimisticF(hand) == 30)
    assert(TwentyOneGame.pessimisticF(hand) == 30)
    assert(TwentyOneGame.determineBetterHandValue(hand) == 30)
  }
  "A Hand made up of 3 aces" should "yield pessimistic=3 optimistic=33 and best=14" in {
    val hand = Array(11, 11, 11)
    assert(TwentyOneGame.optimisticF(hand) == 33)
    assert(TwentyOneGame.pessimisticF(hand) == 3)
    assert(TwentyOneGame.determineBetterHandValue(hand) == 13)
    assert(TwentyOneGame.determineBestestHandValue(hand) == 13)
  }
}
