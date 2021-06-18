package exercise4
import java.util.concurrent.TimeUnit

import autozion._
import zio._

object Task3 extends zio.App {
  val program: ZIO[Any, Nothing, Unit] = for {
    elders <- (ZIO.succeed(Elder) zipPar ZIO.succeed(Elder))
    workers <- (ZIO.succeed(Worker) zipPar ZIO.succeed(Worker))
    overseers <- (ZIO.succeed(Overseer))
    praisers <- (ZIO.succeed(Praiser))
    reporters <- (ZIO.succeed(Reporter))
    robots <- (ZIO.succeed(elders)
      zipPar ZIO.succeed(overseers)
      zipPar ZIO.succeed(workers)
      zipPar ZIO.succeed(praisers)
      zipPar ZIO.succeed(reporters)).fork
    _ <- robots.join
  } yield ()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = program.exitCode

}
