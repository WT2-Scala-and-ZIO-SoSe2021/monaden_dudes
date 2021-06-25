package exercise4

import zio.blocking.{Blocking, effectBlocking}
import zio.clock.Clock
import zio.console._
import zio.duration.{Duration, durationLong}
import zio.random.{Random, nextLongBetween}
import zio.{Has, Schedule, ZIO}

import java.io.IOException


package object autozion {
  type MyEnv = Has[News] with Has[JobBoard] with Has[CompletedJobsHub] with Random with Clock with Blocking with Console
  type ElderEnv = Has[JobBoard] with Random with Clock with Console
  type WorkerEnv = Has[JobBoard] with Has[CompletedJobsHub] with Clock with Blocking
  type OverseerEnv = Has[News] with Has[CompletedJobsHub] with Clock
  type PraiserEnv = Has[News] with Has[CompletedJobsHub] with Clock
  type ReporterEnv = Has[News] with Clock with Console

  sealed trait Job

  trait Robot {
    def name: String

    def work: ZIO[MyEnv, Any, Unit]
  }

  case class PendingJob(name: String, duration: Duration) extends Job

  case class CompletedJob(name: String, completedBy: Robot) extends Job

  case class Elder(name: String = "Elder") extends Robot {

    override def work: ZIO[ElderEnv, Any, Unit] = (for {
      randomLong <- nextLongBetween(2, 5)
      duration = randomLong.seconds
      jobName = s"do something for ${duration.toSeconds.toString} seconds from $name"
      job = PendingJob(jobName, duration)
      _ <- JobBoard.submit(job)
    } yield ()).repeat(Schedule.spaced(2.seconds).delayed(_ => 5.seconds)).unit
  }

  case class Worker(name: String = "Worker") extends Robot {

    override def work: ZIO[WorkerEnv with Console, Any, Unit] = (for {
      job <- JobBoard.take()
      _ <- effectBlocking(Thread.sleep(job.duration.toMillis))
      _ <- CompletedJobsHub.publish(CompletedJob(job.name, this))
    } yield ()).repeat(Schedule.spaced(2.seconds)).unit
  }

  case class Overseer(name: String = "Overseer") extends Robot {

    override def work: ZIO[OverseerEnv with Console, IOException, Unit] = for {
      _ <- CompletedJobsHub.subscribe.use { queue =>
        (for {
          completedJob <- queue.take
          _ <- News.post(s"$name: Job '${completedJob.name}' completed by ${completedJob.completedBy.name}.")
        } yield ()).repeat(Schedule.spaced(1.seconds))
      }
    } yield ()
  }


  case class Praiser(name: String = "Praiser") extends Robot {

    override def work: ZIO[PraiserEnv, Any, Unit] = for {
      _ <- CompletedJobsHub.subscribe.use { queue =>
        (for {
          completedJob <- queue.take
          workerName = completedJob.completedBy.name
          _ <- News.post(s"$name: Thank you $workerName for completing that task!")
        } yield ()).repeat(Schedule.fixed(1.seconds))
      }
    } yield ()
  }

  case class Reporter(name: String = "Reporter") extends Robot {

    override def work: ZIO[ReporterEnv, Any, Unit] = (for {
      news <- News.proclaim
      _ <- putStrLn(s"Breaking News from $name: '$news'")
    } yield ()).repeat(Schedule.spaced(1.seconds)).unit
  }
}
