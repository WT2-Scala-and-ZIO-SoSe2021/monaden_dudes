package exercise4

import zio.blocking.{Blocking, effectBlocking}
import zio.clock.Clock
import zio.duration.{Duration, durationInt}
import zio.random._
import zio.{Dequeue, Has, Hub, Queue, Schedule, UIO, ULayer, URIO, ZIO, ZLayer, ZManaged}

package object AutoZion {
  type MyEnv = Has[News] with Has[JobBoardLive] with Has[CompletedJobsHubLive]

  trait Robot[-E] {
    def name: String

    def work(): ZIO[E, Any, Unit] // todo: how do we make the ZIO type more generic?
  }

  sealed trait Job

  case class PendingJob(duration: Duration) extends Job

  case class CompletedJob(completedBy: String) extends Job

  // region JobBoard

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

  case class JobBoardLive() extends JobBoard {
    val jobQueue: UIO[Queue[PendingJob]] = Queue.unbounded[PendingJob]

    override def submit(job: PendingJob): UIO[Unit] = jobQueue.map(x => x.offer(job))

    override def take(): UIO[PendingJob] = for {
      queue <- jobQueue
      job_fiber <- queue.poll
      job = job_fiber.get
    } yield job
  }

  object JobBoardLive {
    val layer: ULayer[Has[JobBoard]] = ZLayer.succeed(JobBoardLive())
  }

  // endregion

  // region CompletedJobsHub

  trait CompletedJobsHub {
    def subscribe: ZManaged[Any, Nothing, Dequeue[CompletedJob]]

    def publish(job: CompletedJob): UIO[Unit]
  }

  object CompletedJobsHub {
    def subscribe: URIO[Has[CompletedJobsHub], Dequeue[CompletedJob]] = ZIO.serviceWith[CompletedJobsHub](_.subscribe.useNow)

    def publish(job: CompletedJob): URIO[Has[CompletedJobsHub], Unit] = ZIO.serviceWith[CompletedJobsHub](_.publish(job))
  }

  case class CompletedJobsHubLive() extends JobBoard {
    val jobHub: UIO[Hub[CompletedJob]] = Hub.unbounded[CompletedJob]
    //    val jobQueue: UIO[Queue[CompletedJob]] = Queue.unbounded[CompletedJob]

    override def subscribe: ZManaged[Any, Nothing, Dequeue[CompletedJob]] = for {
      queue <- Queue.unbounded[CompletedJob]
    } yield ZManaged.succeed(queue) // todo: how do we get the correct type?

    // override def subscribe: ZManaged[Any, Nothing, Dequeue[CompletedJob]] = for {
    //   hub <- jobHub
    //   zMan <- hub.subscribe
    // } yield zMan

    def publish(job: CompletedJob): UIO[Unit] = for {
      hub <- jobHub
      _ = hub.publish(job)
    } yield () //jobHub.map(h => h.publish(job))
  }

  object CompletedJobsHubLive {
    val layer: ULayer[Has[JobBoard]] = ZLayer.succeed(JobBoardLive())
  }

  // endregion

  // region News

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
      latest <- queue.poll.map(x => x.get)
    } yield latest
  }

  object NewsLive {
    val layer: ULayer[Has[News]] = ZLayer.succeed(NewsLive())
  }

  // endregion

  // region Robots

  type ElderEnv = Has[JobBoard] with Random with Clock

  case class Elder() extends Robot[ElderEnv] {
    override def name: String = ???

    override def work(): ZIO[ElderEnv, Any, Unit] = {
      val action = for {
        randomInt <- nextIntBetween(1, 10)
        _ <- JobBoard.submit(PendingJob(randomInt.seconds))
      } yield ()

      val policy = Schedule.fixed(2.seconds)

      (action repeat policy).unit
    }
  }

  type WorkerEnv = Has[CompletedJobsHub] with Has[JobBoard] with Clock with Blocking

  case class Worker() extends Robot[WorkerEnv] {
    override def name: String = ???

    override def work(): ZIO[WorkerEnv, Any, Unit] = {
      val action = for {
        job <- JobBoard.take()
        _ <- effectBlocking(Thread.sleep(job.duration.toMillis))
        _ <- CompletedJobsHub.publish(CompletedJob(name))
      } yield ()

      val policy = Schedule.fixed(2.seconds)

      (action repeat policy).unit
    }

    // todo: implement me
    case class Overseer() extends Robot {
      override def name: String = ???

      override def work(): ZIO[MyEnv, Any, Unit] = ???
    }

    // todo: implement me
    case class Praiser() extends Robot {
      override def name: String = ???

      override def work(): ZIO[MyEnv, Any, Unit] = ???
    }

    // todo: implement me
    case class Reporter() extends Robot {
      override def name: String = ???

      override def work(): ZIO[MyEnv, Any, Unit] = ???
    }

    // endregion

  }
