package exercise4.autozion

import zio.{Dequeue, Has, Hub, Managed, UIO, ULayer, URIO, ZIO, ZLayer, ZManaged}


trait CompletedJobsHub {
  def subscribe: ZManaged[Any, Nothing, Dequeue[CompletedJob]]

  def publish(job: CompletedJob): UIO[Unit]
}

case class CompletedJobsHubLive(hub: Hub[CompletedJob]) extends CompletedJobsHub {

  override def subscribe: ZManaged[Any, Nothing, Dequeue[CompletedJob]] = {
    hub.subscribe
  }

  override def publish(job: CompletedJob): UIO[Unit] = {
    hub.publish(job).unit
  }
}

object CompletedJobsHub {
  def subscribe: ZManaged[Has[CompletedJobsHub], Nothing, Dequeue[CompletedJob]] = Managed.accessManaged[Has[CompletedJobsHub]](_.get.subscribe)

  def publish(job: CompletedJob): URIO[Has[CompletedJobsHub], Unit] = ZIO.serviceWith[CompletedJobsHub](_.publish(job))
}

object CompletedJobsHubLive {
  val layer: ULayer[Has[CompletedJobsHub]] = ZLayer.fromEffect({
    val hubIO: UIO[Hub[CompletedJob]] = Hub.unbounded[CompletedJob]
    hubIO.map(x => CompletedJobsHubLive(x))
  })
}

