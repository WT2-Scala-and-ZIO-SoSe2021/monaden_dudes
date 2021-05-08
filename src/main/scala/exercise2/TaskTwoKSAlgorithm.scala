import lib.StdAudio

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random
object TaskTwoKSAlgorithm {
  def whiteNoise(frequency: Int = 440, volume: Double = 1.0): Queue[Double] = {
    val r = Random
    List.fill(frequency)(0.0)
      .map(_ => r.between(-.5, .5)*volume)
      .to(scala.collection.immutable.Queue)
  }

  def update(currentQueue: Queue[Double]): Queue[Double] = {
    val (frontElem, newQueue) = currentQueue.dequeue
    val nextFrontElem = newQueue.front
    val newValue = ((frontElem+nextFrontElem)/2) * 0.996
    newQueue.appended(newValue)
  }

  def loop(audioQueue: Queue[Double], audioFunction: Double=>Unit) = {
    var currentQueue = audioQueue
    while (true) {
      currentQueue = update(currentQueue)
      audioFunction(currentQueue.last)
    }

  }

  def play(double: Double) = {
    StdAudio.play(Array(double))
  }



  def main(args: Array[String]): Unit = {
    val queue = whiteNoise()
    loop(queue, play)
  }

}