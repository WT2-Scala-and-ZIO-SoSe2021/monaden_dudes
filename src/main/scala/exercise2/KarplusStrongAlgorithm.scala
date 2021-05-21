package exercise2

import scala.collection.immutable.Queue
import scala.util.Random
import lib.StdAudio

import scala.annotation.tailrec

object KarplusStrongAlgorithm {

  val EnergyDecayFactor = 0.996
  val fundamentalFrequency = 440

  def whiteNoise(frequency: Double = 440.0, volume: Double = 1.0): Queue[Double] = {
    val realFrequency = (fundamentalFrequency * (fundamentalFrequency.toDouble/frequency))
    val r = new Random()
    (1 to realFrequency.toInt)
      .map(_ => r.between(-0.5, 0.5) * volume)
      .to(Queue)
  }

  def update(queue: Queue[Double]): Queue[Double] = {
    val (head, tailQueue) = queue.dequeue
    val tailHead = tailQueue.front
    val newDouble = ((head + tailHead) / 2) * EnergyDecayFactor

    tailQueue.appended(newDouble)
  }

  def loop(playFunc: Double => Unit)(queue: Queue[Double]): Unit = {
    @tailrec
    def _loop(queue: Queue[Double]): Unit = {
      val newQueue = update(queue)
      playFunc(newQueue.head)
      //println("avg queue value", newQueue.sum / 440)
      _loop(newQueue)
    }

    _loop(queue)
  }

  def main(args: Array[String]): Unit = {
    loop(StdAudio.play)(whiteNoise(frequency = 16.0))
  }

}
