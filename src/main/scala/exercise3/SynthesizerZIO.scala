package exercise3
import java.io.IOException

import exercise2.KarplusStrongAlgorithm.{loop, update, whiteNoise}
import lib.StdAudio
import zio.{UIO, URIO, ZIO}
import zio.console._
import zio.random.{nextDoubleBetween, _}
import zio._

import scala.annotation.tailrec
import scala.collection.immutable.Queue


object SynthesizerZIO extends zio.App {

  val EnergyDecayFactor = 0.996
  val fundamentalFrequency = 440.0

  def whiteNoise(frequency: Double = 440.0, volume: Double = 1.0): URIO[Random, Queue[Double]] = {
    val realFrequency = (fundamentalFrequency * (fundamentalFrequency/frequency))
    nextDoubleBetween(-0.5, 0.5)
      .map(r => volume * r)
      .replicateM(realFrequency.toInt)
      .map(x => x.to(Queue))
  }


//  def update(queue: Queue[Double]): Queue[Double] = {
//    val (head, tailQueue) = queue.dequeue
//    val tailHead = tailQueue.front
//    val newDouble = ((head + tailHead) / 2) * EnergyDecayFactor
//
//    tailQueue.appended(newDouble)
//  }

  def loop(queue: Queue[Double]): ZIO[Random, Throwable, Unit] = {
    @tailrec
    def _loop(queue: Queue[Double]): Unit = {
      val newQueue = update(queue)
//      play(newQueue.head).run
      StdAudio.play(newQueue.head)
      _loop(newQueue)
    }
    ZIO.succeed(_loop(queue))
  }


  def play(sample: Double) : UIO[Unit] =
    UIO.succeed(StdAudio.play(sample))

  def run(args: List[String]): URIO[Random with Console, ExitCode] = {
    for {
      q <- whiteNoise(frequency = 440.0, volume = 0.5)
      poop <- loop(q).exitCode
    } yield poop

  }

}
