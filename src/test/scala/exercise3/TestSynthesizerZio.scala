package exercise3

import exercise3.SynthesizerZIO.whiteNoise
import zio.test.Assertion.{approximatelyEquals, equalTo}
import zio.test.environment.TestRandom
import zio.test.{DefaultRunnableSpec, assert}

object TestSynthesizerZio extends DefaultRunnableSpec {
  def spec = suite("WhiteNoise Test")(
    testM("WhiteNoise without parameters should return array of Random Double Values between -0.5 and 0.5") {
      for {
        queue <- whiteNoise()
      } yield assert(queue.size)(equalTo(440)) &&
        assert(queue.head)(approximatelyEquals(0, 0.5)) &&
        assert(queue.last)(approximatelyEquals(0, 0.5))
    },
    testM("WhiteNoise with frequency parameter should return queue of different length") {
      val newFrequency: Double = 220.0
      val newFrequencyQueueLength: Int = (440.0 * (440.0 / newFrequency)).toInt
      for {
        queue <- whiteNoise(frequency = newFrequency)
      } yield assert(queue.size)(equalTo(newFrequencyQueueLength)) &&
        assert(queue.head)(approximatelyEquals(0, 0.5))
    },
    testM("WhiteNoise with different volume parameter should return queue with appropriately different values") {
      for {
        queue <- whiteNoise(volume = 40)
      } yield assert(queue.head)(approximatelyEquals(0, 20.0)) &&
        assert(queue.last)(approximatelyEquals(0, 20.0))
    },
    testM("WhiteNoise with random feed should return the preset elements") {
      for {
        _ <- TestRandom.feedDoubles(0.4, -0.4, 0.2, 0.3)
        queue <- whiteNoise()
      } yield assert(queue.head)(equalTo(0.4)) &&
        assert(queue.tail.head)(equalTo(-0.4)) &&
        assert(queue.tail.tail.head)(equalTo(0.2)) &&
        assert(queue.tail.tail.tail.head)(equalTo(0.3))
    })
}
