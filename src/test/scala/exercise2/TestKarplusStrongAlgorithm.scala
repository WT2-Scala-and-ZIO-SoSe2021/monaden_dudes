package exercise2

import exercise2.KarplusStrongAlgorithm.{loop, whiteNoise}
import org.scalactic.TolerantNumerics
import org.scalatest.exceptions.{TestCanceledException, TestFailedException}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.Queue

class TestKarplusStrongAlgorithm extends AnyFlatSpec{

  "Generating whiteNoise with no args" should "return a Queue with 440 elements" in {
    val noiseQueue = KarplusStrongAlgorithm.whiteNoise()
    assert(noiseQueue.length == 440)
    assert((noiseQueue.min >= -0.5))
    assert((noiseQueue.max <= 0.5))
  }

  "Generating whiteNoise with frequency 220 and volume 2" should "return a Queue with 220 elements between -1.0 and 1.0" in {
    val noiseQueue = KarplusStrongAlgorithm.whiteNoise(frequency = 220, volume = 2.0)
    assert(noiseQueue.length == 220)
    assert((noiseQueue.min >= -1.0))
    assert((noiseQueue.max <= 1.0))
  }

  "Update on a queue with two ones at the front" should "result in a queue with the EnergyDecayFactor at the end and a one in front" in {
    val noiseQueue = Queue(1.0, 1.0, 2.0, 2.0, 2.0, 2.0)

    val updatedQueue = KarplusStrongAlgorithm.update(noiseQueue)
    assert(updatedQueue.length == 6)
    assert(updatedQueue.last == KarplusStrongAlgorithm.EnergyDecayFactor)
    assert(updatedQueue.front == 1.0)
  }

  "2000000 loop runs" should "result in a near zero double value for the playFunc" in {
    val epsilon = 1e-8f
    implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

    var counter = 0
    def loopCounter(value: Double): Unit = {
      if (counter >= 2000000) {
        assert(value === 0.0)
        cancel()  // workaround to abort the test
      }
      counter += 1
    }

    assertThrows[TestCanceledException] {
      loop(loopCounter)(whiteNoise())
    }
  }
}
