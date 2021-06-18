package exercise4

import zio.{ExitCode, URIO, ZIO}

object Task2 extends zio.App {

    val program = for {
      //fib <- (ZIO.fail("e1") zipPar ZIO.fail("e2").ensuring(ZIO.die(new RuntimeException("hi")))).fork
//      fib <- (ZIO.fail("e1") zipPar ZIO.die(new RuntimeException("hi"))).fork

//      fib0 <- (ZIO.fail("egh") zipPar ZIO.fail("Error")).fork
//      fib1 <- (fib0.join zipPar ZIO.fail("Error").ensuring(fib0.interrupt)).fork
//      fib1 <- (ZIO.succeed("aa")).fork
//      fib2 <- (ZIO.succeed("aaa")).fork
//      fib3 <- (ZIO.fail("e2").ensuring(fib0.interrupt)).fork
//      res_1 <- fib1.join zipPar fib2.join
//      res_3 <- fib3.join
//      res_0 <- fib0.join
//      res_1 <- fib1.join

      // TODO change this to work a little better
      fiber1 <- ZIO.fail("uh oh stinky").fork
      fiber2 <- ZIO.fail("buh boh binky").fork
      fiber3 = fiber1.zip(fiber2)
      fiber4 <- ZIO.fail("Error").fork
      fiber5 = fiber3.zip(fiber4)
      a <- fiber5.join

//      fib0 <- ZIO.fail("buh boh binky").fork
//      fib1 <- (ZIO.fail("uh oh stinky") zipPar fib0.join).fork
//      fib2 <- (ZIO.fail("Errorre")).fork
//
//      a <- fib3.join
////      a <- "aaaa"

    } yield a

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = program.exitCode
}
