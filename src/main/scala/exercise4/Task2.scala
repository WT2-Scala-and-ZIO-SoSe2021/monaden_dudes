package exercise4

import zio.{ExitCode, URIO, ZIO}

object Task2 extends zio.App {

    val program = for {
      //fib <- (ZIO.fail("e1") zipPar ZIO.fail("e2").ensuring(ZIO.die(new RuntimeException("hi")))).fork
//      fib <- (ZIO.fail("e1") zipPar ZIO.die(new RuntimeException("hi"))).fork

      fib0 <- (ZIO.fail("e1")).fork
      fib1 <- (ZIO.succeed("aa")).fork
      fib2 <- (ZIO.succeed("aaa")).fork

      fib3 <- (ZIO.fail("e2").ensuring(fib0.interrupt)).fork
      res_1 <- fib1.join zipPar fib2.join
      res_3 <- fib3.await
      a = 5
    } yield()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = program.exitCode
}
