package exercise3

import exercise3.SynthesizerZIO.whiteNoise

import zio.URIO
import zio.random._
import zio.test.Assertion.{anything, approximatelyEquals, equalTo}
import zio.test.{DefaultRunnableSpec, assertM, suite, typeCheck, assert}
import zio.test.environment.TestRandom

import scala.collection.immutable.Queue

object TestSynthesizerZio extends DefaultRunnableSpec {
  def spec = suite("WhiteNoise Test")(
    testM("WhiteNoise without parameters should return array of Random Double Values between -0.5 and 0.5"){
      val whiteNoiseQueue: URIO[Random, Queue[Double]] = whiteNoise()
      assertM(whiteNoiseQueue.map(q=>q.size))(equalTo(440))
      assertM(whiteNoiseQueue.map(q=>q.head))(approximatelyEquals(0, 0.5))
      assertM(whiteNoiseQueue.map(q=>q.last))(approximatelyEquals(0, 0.5))
    },
    testM("WhiteNoise with frequency parameter should return queue of different length"){
      val newFrequency:Double = 220.0
      val newFrequencyQueueLength:Int = (440.0 * (440.0 / newFrequency)).toInt
      val whiteNoiseQueue: URIO[Random, Queue[Double]] = whiteNoise(frequency = newFrequency)
      assertM(whiteNoiseQueue.map(q=>q.size))(equalTo(newFrequencyQueueLength))
      assertM(whiteNoiseQueue.map(q=>q.head))(approximatelyEquals(0, 0.5))
    },
    testM("WhiteNoise with different volume parameter should return queue with appropriately different values"){
      val whiteNoiseQueue: URIO[Random, Queue[Double]] = whiteNoise(volume=40)
      assertM(whiteNoiseQueue.map(q=>q.head))(approximatelyEquals(0, 20.0))
      assertM(whiteNoiseQueue.map(q=>q.last))(approximatelyEquals(0, 20.0))
    },
    testM("WhiteNoise with frequency parameter should return queue of different length"){
    for {
        _ <- TestRandom.feedDoubles(0.4, -0.4, 0.2, 0.3)
        queue <- whiteNoise()

      } yield assert(queue.head)(equalTo(0.4)) &&
              assert(queue.tail.head)(equalTo(-0.4)) &&
              assert(queue.tail.tail.head)(equalTo(0.2))
    })
}
