import zio.blocking.{Blocking, effectBlocking}
import zio.clock.Clock
import zio.random.{Random, nextLong, nextString}
import zio.{Dequeue, DurationSyntax, ExitCode, Has, Hub, Queue, Schedule, UIO, ULayer, URIO, ZEnv, ZIO, ZLayer, ZManaged}
import zio.duration.{Duration, durationLong}
import zio.console._

import java.io.IOException

package object autozion {
  type MyEnv = Has[News] with Has[JobBoard] with Has[CompletedJobsHub]

  trait Robot[-E] {
    def name: String
    def work(): ZIO[E, Any, Unit]
  }

  sealed trait Job

  case class PendingJob(duration: Duration) extends Job
  case class CompletedJob(completedBy: String) extends Job


  trait JobBoard {
    /**
     * Submits a job to the job board, which can later be taken up by a robot using the take method.
     */
    def submit(job: PendingJob): UIO[Unit]

    /**
     * Take a job from the job board
     */
    def take(): UIO[PendingJob]
  }

  object JobBoard {
    def submit(job: PendingJob): URIO[Has[JobBoard], Unit] = ZIO.serviceWith[JobBoard](_.submit(job))

    def take(): URIO[Has[JobBoard], PendingJob] = ZIO.serviceWith[JobBoard](_.take())
  }

  case class JobBoardLive() extends JobBoard{
    val jobQueue: UIO[Queue[PendingJob]] = Queue.unbounded[PendingJob]
    override def submit(job: PendingJob): UIO[Unit] = jobQueue.map(x=>x.offer(job))
    override def take(): UIO[PendingJob] = for {
      queue <- jobQueue
      job_fiber <- queue.poll
      job = job_fiber.get
    } yield job
  }

  object JobBoardLive {
    val layer: ULayer[Has[JobBoard]] = ZLayer.succeed(JobBoardLive())
  }

  trait CompletedJobsHub {
    def subscribe: ZManaged[Any, Nothing, Dequeue[CompletedJob]]

    def publish(job: CompletedJob): UIO[Unit]
  }

  object CompletedJobsHub {
    def subscribe: URIO[Has[CompletedJobsHub], Dequeue[CompletedJob]] = ZIO.serviceWith[CompletedJobsHub](_.subscribe.useNow)

    def publish(job: CompletedJob): URIO[Has[CompletedJobsHub], Unit] = ZIO.serviceWith[CompletedJobsHub](_.publish(job))
  }

  case class CompletedJobsHubLive() extends CompletedJobsHub{
    val jobHub: UIO[Hub[CompletedJob]] = Hub.unbounded[CompletedJob]

    override def subscribe: ZManaged[Any, Nothing, Dequeue[CompletedJob]] = ???
//      for {
//      hub <- jobHub
//      zMan <- hub.subscribe
//    } yield zMan

    override def publish(job: CompletedJob): UIO[Unit] = for {
      hub <- jobHub
      _ <- hub.publish(job)
    } yield ()
  }

  object CompletedJobsHubLive {
    val layer: ULayer[Has[JobBoard]] = ZLayer.succeed(JobBoardLive())
  }

  trait News {
    def post(news: String): UIO[Unit]

    def proclaim(): UIO[String]
  }

  object News {
    def post(news: String): URIO[Has[News], Unit] = ZIO.serviceWith[News](_.post(news))

    def proclaim: URIO[Has[News], String] = ZIO.serviceWith[News](_.proclaim())
  }

  case class NewsLive() extends News {
    val newsQueue: UIO[Queue[String]] = Queue.unbounded[String]

    override def post(news: String): UIO[Unit] = for {
      queue <- newsQueue
      _ <- queue.offer(news)
    } yield ()

    override def proclaim(): UIO[String] = for {
      queue <- newsQueue
      latest <- queue.poll.map(x=>x.get)
    } yield latest
  }

  object NewsLive {
    val layer: ULayer[Has[News]] = ZLayer.succeed(NewsLive())
  }

  type ElderEnv = Has[JobBoard] with Random with Clock
  type WorkerEnv = Has[JobBoard] with Has[CompletedJobsHub] with Clock with Blocking
  type OverseerEnv = Has[News] with Has[CompletedJobsHub] with Clock
  type PraiserEnv = Has[News] with Has[CompletedJobsHub] with Clock
  type ReporterEnv = Has[News] with Clock with Console

  case class Elder() extends Robot[ElderEnv]{
    override def name: String = {
      val randomString = for {
        randomString <- nextString(5)
      } yield randomString
      s"Elder_$randomString"
    }

    override def work(): ZIO[ElderEnv, Any, Unit] = {
      val action: ZIO[Has[JobBoard] with Random, Nothing, Unit] = for {
        randomLong <- nextLong
        duration = randomLong.seconds
        job = PendingJob(duration)
        _ <- JobBoard.submit(job)
      } yield ()

      val policy: Schedule[Any, Any, Long] = Schedule.fixed(2.seconds)
      (action repeat policy).unit
    }
  }

  case class Worker() extends Robot[WorkerEnv]{
    override def name: String = {
      val randomString = for {
        randomString <- nextString(5)
      } yield randomString
      s"Worker_$randomString"
    }

    override def work(): ZIO[WorkerEnv, Any, Unit] = {
      val action = for {
        job <- JobBoard.take()
        _ <- effectBlocking(Thread.sleep(job.duration.toMillis))
        _ <- CompletedJobsHub.publish(CompletedJob(name))
      } yield()

      val policy = Schedule.fixed(2.seconds)

      (action repeat policy).unit
    }
  }

  case class Overseer() extends Robot[OverseerEnv]{
    override def name: String = {
      val randomString = for {
        randomString <- nextString(5)
      } yield randomString
      s"Overseer_$randomString"
    }

    override def work(): ZIO[OverseerEnv, Any, Unit] = {
      val action = for {
        jobSubscription <- CompletedJobsHub.subscribe
        completedJob <- jobSubscription.take
        jobName = completedJob.toString
        workerName = completedJob.completedBy
        _ <- News.post(s"Job $jobName completed by $workerName.")
      } yield()
      val policy = Schedule.fixed(1.seconds)

      (action repeat policy).unit
    }
  }

  case class Praiser() extends Robot[PraiserEnv] {
    override def name: String = {
      val randomString = for {
        randomString <- nextString(5)
      } yield randomString
      s"Praiser_$randomString"
    }

    override def work(): ZIO[PraiserEnv, Any, Unit] = {
      val action = for {
        jobSubscription <- CompletedJobsHub.subscribe
        completedJob <- jobSubscription.take
        workerName = completedJob.completedBy
        _ <- News.post(s"Thank you $workerName for completing that task!")
      } yield()

      val policy = Schedule.fixed(2.seconds)

      (action repeat policy).unit
    }
  }

  case class Reporter() extends Robot[ReporterEnv]{
    override def name: String = {
      val randomString = for {
        randomString <- nextString(5)
      } yield randomString
      s"Reporter_$randomString"
    }

    override def work(): ZIO[ReporterEnv, IOException, Unit] = {
      val action = for {
        news <- News.proclaim
        _ <- putStrLn(s"Breaking News! $news")
      } yield()
      val policy = Schedule.fixed(1.seconds)

      (action repeat policy).unit
    }
  }
}
