package exercise4
import java.util.concurrent.TimeUnit

import autozion._
import zio._
import zio.duration.{Duration, durationInt}

object Task3 extends zio.App {
  val program: ZIO[MyEnv, Any, Unit] = {
    val elder1 = Elder("Elder John")
    val elder2 = Elder("Elder Johnson")
    val worker1 = Worker("Worker Johnathan")
    val worker2 = Worker("Worker Jebediah")
    val overseer = Overseer("Overseer Jay Leno")
    val praiser = Praiser("Praiser Jacob")
    val reporter = Reporter("Reporter Jojo")

      for {
      _ <- worker1.work.fork
      _ <- worker2.work.fork
      _ <- ZIO.sleep (Duration.fromMillis (100))
      _ <- elder1.work.fork
      _ <- elder2.work.fork
      _ <- overseer.work.fork
      _ <- praiser.work.fork
      _ <- reporter.work
      } yield ()
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = program.provideCustomLayer(NewsLive.layer ++ JobBoardLive.layer ++ CompletedJobsHubLive.layer).exitCode

}
