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
    val realFrequency = (fundamentalFrequency * (fundamentalFrequency/frequency)).toInt
    nextDoubleBetween(-0.5, 0.5)
      .map(r => volume * r)
      .replicateM(realFrequency.toInt)
      .map(x => x.to(Queue))
//    ZIO.foreach((0 to realFrequency).toArray)(_ => nextDoubleBetween(-0.5, 0.5)).map(x => x.to(Queue))
  }


  def loop(queue: Queue[Double]): ZIO[Random, Throwable, Unit] = {
    for {
      newQueue <- ZIO.succeed(update(queue))
      _ <- play(newQueue.head)
      _ <- loop(newQueue)
    } yield()

  }

  def play(sample: Double) : UIO[Unit] =
    ZIO.effectTotal(StdAudio.play(sample))

  def run(args: List[String]): URIO[Random with Console, ExitCode] = {
    for {
      q <- whiteNoise(frequency = 440.0, volume = 0.5)
      poop <- loop(q).exitCode
    } yield poop

  }

}
