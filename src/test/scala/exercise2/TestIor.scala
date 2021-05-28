package exercise2

import org.scalatest.flatspec.AnyFlatSpec

class TestIor extends AnyFlatSpec {
  "A call of Ior.left()" should "generate a Ior with only an error" in {
    val ior = Ior.left[Int, Nothing](2)
    assert(ior.error == 2)
    assert(ior.isInstanceOf[Left[Int, Nothing]])
  }

  "A call of Ior.right()" should "generate a Ior with only an element" in {
    val ior = Ior.right[Nothing, Int](2)
    assert(ior.element == 2)
    assert(ior.isInstanceOf[Right[Nothing, Int]])
  }

  "A call of Ior.both()" should "generate a Ior with an element and error" in {
    val ior = Ior.both[String, Int]("Alarm!", 2)
    assert(ior.error == "Alarm!")
    assert(ior.element == 2)
    assert(ior.isInstanceOf[Both[String, Int]])
  }

  "map on a Ior with value 2 with a function which multiplies elements by 4" should "return a Ior with 8" in {
    val oriIor = Ior.right[Nothing, Int](2)
    val mappedIor = oriIor.map(x => x * 4)

    assert(mappedIor.isInstanceOf[Right[Nothing, Int]])
    // todo: can the cast somehow be omitted?
    assert(mappedIor.asInstanceOf[Right[Nothing, Int]].element == 8)
  }

  "flatMap on a Ior with a int and a function which returns a string" should "return a Ior with a string" in {
    val oriIor = Ior.right[Nothing, Int](2)
    val mappedIor = oriIor.flatMap(_ => Ior.right("a string")) //Right("a string")

    assert(mappedIor.isInstanceOf[Right[Nothing, String]])
    // todo: can the cast somehow be omitted?
    assert(mappedIor.asInstanceOf[Right[Nothing, String]].element == "a string")
  }

  "flatMap on a Right with a function which returns a Left" should "return a Left with the error" in {
    val oriIor = Ior.right[Throwable, Int](2)
    val mappedIor = oriIor.flatMap(_ => Ior.left[Throwable, Int](new RuntimeException("a grave error")))

    assert(mappedIor.isInstanceOf[Left[Throwable, Int]])
    assert(mappedIor.asInstanceOf[Left[Throwable, Int]].error.getMessage == "a grave error")
  }

  "map on a Left" should "return the same Left" in {
    val oriIor = Ior.left[Throwable, String](new RuntimeException("a grave error"))
    val mappedIor = oriIor.map(x => x + "something") //Left(java.lang.RuntimeException: a grave error)

    assert(mappedIor.isInstanceOf[Left[Throwable, String]])
    assert(mappedIor.asInstanceOf[Left[Throwable, String]].error.getMessage == "a grave error")
  }

  val both: Both[Throwable, Int] = Ior.both(
    new RuntimeException("not fatal"), 21)

  "A Both" should "return a Both after map" in {
    val mappedBoth = both.map(x => x * 2)
    assert(mappedBoth.isInstanceOf[Both[Throwable, Int]])
    assert(mappedBoth.asInstanceOf[Both[Throwable, Int]].error.getMessage == "not fatal")
    assert(mappedBoth.asInstanceOf[Both[Throwable, Int]].element == 42)
  }

  it should "return a Left after flatMap which returns a Left" in {
    val mappedBoth = both
      .flatMap(_ => Ior.left[Throwable, Int](new RuntimeException("fatal error")))

    assert(mappedBoth.isInstanceOf[Left[Throwable, Int]])
    assert(mappedBoth.asInstanceOf[Left[Throwable, Int]].error.getMessage == "fatal error")
  }

  it should "return a Both (with mapped value and old error) after flatMap which returns a Right" in {
    val mappedBoth = both
      .flatMap(_ => Ior.right[Throwable, Int](480))

    assert(mappedBoth.isInstanceOf[Both[Throwable, Int]])
    assert(mappedBoth.asInstanceOf[Both[Throwable, Int]].error.getMessage == "not fatal")
    assert(mappedBoth.asInstanceOf[Both[Throwable, Int]].element == 480)
  }

  it should "return a Both with new values after flatMap which returns a Both" in {
    val mappedBoth = both.flatMap(x => Ior.both(new RuntimeException("another not fatal"), x * 3))

    assert(mappedBoth.isInstanceOf[Both[Throwable, Int]])
    assert(mappedBoth.asInstanceOf[Both[Throwable, Int]].error.getMessage == "another not fatal")
    assert(mappedBoth.asInstanceOf[Both[Throwable, Int]].element == 63)
  }
}
