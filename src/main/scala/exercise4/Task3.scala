package exercise4
import java.util.concurrent.TimeUnit

import autozion._
import zio._
import zio.duration.durationInt

object Task3 extends zio.App {
  val program: ZIO[MyEnv, Any, Unit] = for {
//    _ <- ZIO.sleep(5.seconds)
    _ <- (Elder().work() zipPar Elder().work()).fork
//    _ <- ZIO.sleep(2.seconds)
    _ <- (Worker().work() zipPar Worker().work()).fork
//    _ <- ZIO.sleep(2.seconds)
    _ <- Overseer().work().fork
    _ <- Praiser().work().fork
    _ <- ZIO.sleep(2.seconds)

    _ <- Reporter().work()
//    robots <- (ZIO.succeed(elders)
//      zipPar ZIO.succeed(overseers)
//      zipPar ZIO.succeed(workers)
//      zipPar ZIO.succeed(praisers)
//      zipPar ZIO.succeed(reporters)).fork
//    _ <- robots.join
  } yield ()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = program.provideCustomLayer(NewsLive.layer ++ JobBoardLive.layer ++ CompletedJobsHubLive.layer).exitCode

}
