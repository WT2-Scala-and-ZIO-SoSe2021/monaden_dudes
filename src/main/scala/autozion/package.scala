import zio.blocking.{Blocking, effectBlocking}
import zio.clock.Clock
import zio.random.{Random, nextLong, nextLongBetween, nextString}
import zio.{Dequeue, DurationSyntax, ExitCode, Has, Hub, Managed, Queue, Schedule, UIO, ULayer, URIO, ZEnv, ZHub, ZIO, ZLayer, ZManaged}
import zio.duration.{Duration, durationLong}
import zio.console._
import java.io.IOException


package object autozion {
  type MyEnv = Has[News] with Has[JobBoard] with Has[CompletedJobsHub] with Random with Clock with Blocking with Console

  trait Robot {
    def name: String
    def work: ZIO[MyEnv, Any, Unit]
  }

  sealed trait Job

  case class PendingJob(name: String, duration: Duration) extends Job
  case class CompletedJob(name: String, completedBy: Robot) extends Job


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

  case class JobBoardLive(queue: Queue[PendingJob]) extends JobBoard{
    override def submit(job: PendingJob): UIO[Unit] = queue.offer(job).unit
    override def take(): UIO[PendingJob] = queue.take
  }

  object JobBoardLive {
    val layer: ULayer[Has[JobBoard]] = ZLayer.fromEffect({
      val queueIO: UIO[Queue[PendingJob]] = Queue.unbounded[PendingJob]
      queueIO.map(x=>JobBoardLive(x))
    })
  }

  trait CompletedJobsHub {
    def subscribe: ZManaged[Any, Nothing, Dequeue[CompletedJob]]

    def publish(job: CompletedJob): UIO[Unit]
  }

  object CompletedJobsHub {
    def subscribe: ZManaged[Has[CompletedJobsHub], Nothing, Dequeue[CompletedJob]] = Managed.accessManaged[Has[CompletedJobsHub]](_.get.subscribe)

    def publish(job: CompletedJob): URIO[Has[CompletedJobsHub], Unit] = ZIO.serviceWith[CompletedJobsHub](_.publish(job))
  }

  case class CompletedJobsHubLive(hub: Hub[CompletedJob]) extends CompletedJobsHub {

    override def subscribe: ZManaged[Any, Nothing, Dequeue[CompletedJob]] = {
      hub.subscribe
    }

    override def publish(job: CompletedJob): UIO[Unit] = {
      hub.publish(job).unit
    }
  }

  object CompletedJobsHubLive {
    val layer: ULayer[Has[CompletedJobsHub]] = ZLayer.fromEffect({
      val hubIO: UIO[Hub[CompletedJob]] = Hub.unbounded[CompletedJob]
      hubIO.map(x=>CompletedJobsHubLive(x))
    })
  }

  trait News {
    def post(news: String): UIO[Unit]

    def proclaim(): UIO[String]
  }

  object News {
    def post(news: String): URIO[Has[News], Unit] = ZIO.serviceWith[News](_.post(news))

    def proclaim: URIO[Has[News], String] = ZIO.serviceWith[News](_.proclaim())
  }

  case class NewsLive(queue: Queue[String]) extends News {
    override def post(news: String): UIO[Unit] = queue.offer(news).unit

    override def proclaim(): UIO[String] = queue.take
  }

  object NewsLive {
    val layer: ULayer[Has[News]] = ZLayer.fromEffect({
      val queueIO: UIO[Queue[String]] = Queue.unbounded[String]
      queueIO.map(x=>NewsLive(x))
    })
  }

  type ElderEnv = Has[JobBoard] with Random with Clock with Console
  type WorkerEnv = Has[JobBoard] with Has[CompletedJobsHub] with Clock with Blocking
  type OverseerEnv = Has[News] with Has[CompletedJobsHub] with Clock
  type PraiserEnv = Has[News] with Has[CompletedJobsHub] with Clock
  type ReporterEnv = Has[News] with Clock with Console

  case class Elder(val name: String = "Elder") extends Robot{
//    override def name: String = {
//      val randomString = for {
//        randomString <- nextString(5)
//      } yield randomString
//      s"Elder_$randomString"
//    }

    override def work: ZIO[ElderEnv, Any, Unit] = {
      val action: ZIO[Has[JobBoard]with Random with Console, Any, Unit] = for {
        randomLong <- nextLongBetween(2, 5)
        duration = randomLong.seconds
        jobName = name + "job" + duration.toString
        job = PendingJob(jobName, duration)
//        _ <- putStrLn(s"${this.name} putting job ${jobName} on the Board")
        _ <- JobBoard.submit(job)
      } yield ()

      val policy: Schedule[Any, Any, Long] = Schedule.spaced(2.seconds).delayed(_ => 5.seconds)
      (action repeat policy).unit
    }
  }

  case class Worker(val name: String = "Worker") extends Robot{
//    override def name: String = {
//      val randomString = for {
//        randomString <- nextString(5)
//      } yield randomString
//      s"Worker_$randomString"
//    }

    override def work: ZIO[WorkerEnv with Console, Any, Unit] = {
      val action = for {
        job <- JobBoard.take()
//        _ <- putStrLn(s"${this.name} recieved job ${job.name}, commencing work")
        _ <- effectBlocking(Thread.sleep(job.duration.toMillis))
        _ <- CompletedJobsHub.publish(CompletedJob(job.name, this))
      } yield()

      val policy = Schedule.spaced(2.seconds)

      (action repeat policy).unit
    }
  }

  case class Overseer(val name: String = "Overseer") extends Robot{
//    override def name: String = {
//      val randomString = for {
//        randomString <- nextString(5)
//      } yield randomString
//      s"Overseer_$randomString"
//    }

    override def work: ZIO[OverseerEnv with Console, IOException, Unit] = {
      val action = for {
//        _ <- putStrLn(s"Overseer $name kicking into action")
        jobSubscription <- CompletedJobsHub.subscribe
//        _ <- putStrLn(s"Connection to CompletedJobsHub $jobSubscription. established")
//        subscriptionSize <- jobSubscription.size
        completedJob = jobSubscription.take
//        _ <- putStrLn(s"found completed Job $completedJob")
//        _ <- putStrLn(s"Overseer $name overseeing worker $workerName activity on job $jobName")
        _ <- News.post(s"Job ${completedJob.map(_.name)} completed by ${completedJob.map(_.name)}.").toManaged_
      } yield()
      val policy = Schedule.spaced(1.seconds)

      (action.useNow repeat policy).unit
    }
  }

  case class Praiser(val name: String = "Praiser") extends Robot{
//    override def name: String = {
//      val randomString = for {
//        randomString <- nextString(5)
//      } yield randomString
//      s"Praiser_$randomString"
//    }

    override def work: ZIO[PraiserEnv, Any, Unit] = {
      val action = for {
        jobSubscription <- CompletedJobsHub.subscribe
        completedJob <- jobSubscription.take.toManaged_
        workerName = completedJob.completedBy.name
        _ <- News.post(s"Thank you $workerName for completing that task!").toManaged_
      } yield()

      val policy = Schedule.fixed(1.seconds)

      (action.useNow repeat policy).unit
    }
  }

  case class Reporter(val name: String = "Reporter") extends Robot{
//    override def name: String = {
//      val randomString = for {
//        randomString <- nextString(5)
//      } yield randomString
//      s"Reporter_$randomString"
//    }

    override def work: ZIO[ReporterEnv, Any, Unit] = {
      val action = for {
//        _ <- putStrLn("Breaking News! ")
        news <- News.proclaim
        _ <- putStrLn(s"$news")
      } yield()
      val policy = Schedule.spaced(1.seconds)

      (action repeat policy).unit
    }
  }
}
