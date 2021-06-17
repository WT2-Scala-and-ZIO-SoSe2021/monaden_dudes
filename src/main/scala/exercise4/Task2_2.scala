package exercise4

import zio.{ExitCode, IO, URIO}

object Task2_2 extends zio.App {
  // it kinda works
  val program = for {
    fiber1 <- IO.fail("Uh oh!").fork
    fiber2 <- IO.fail("Hurray!").fork
    fiber12 = fiber1.zip(fiber2)
    fiber3 <- IO.fail("murph!").fork
    fiber123 = fiber12.zip(fiber3)

    tuple <- fiber123.join
  } yield tuple

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = program.exitCode
}
