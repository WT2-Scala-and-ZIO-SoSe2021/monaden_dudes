package exercise4
import exercise4.autozion._
import zio._
import zio.duration.Duration

object Task3 extends zio.App {
  val program: ZIO[MyEnv, Any, Unit] = {
    val elder1 = Elder("[Elder] Edward")
    val elder2 = Elder("[Elder] Eva")
    val worker1 = Worker("[Worker] Walter")
    val worker2 = Worker("[Worker] Winston")
    val overseer = Overseer("[Overseer] Olaf")
    val praiser = Praiser("[Praiser] Patrick")
    val reporter = Reporter("[Reporter] Rudolph")

      for {
      _ <- worker1.work.fork
      _ <- worker2.work.fork
      _ <- elder1.work.fork
      _ <- elder2.work.fork
      _ <- overseer.work.fork
      _ <- praiser.work.fork
      _ <- reporter.work
      } yield ()
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = program.provideCustomLayer(NewsLive.layer ++ JobBoardLive.layer ++ CompletedJobsHubLive.layer).exitCode

}
