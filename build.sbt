name := "wt2"

version := "0.1"

scalaVersion := "2.13.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test"
libraryDependencies += "dev.zio" %% "zio" % "1.0.8"
libraryDependencies ++= Seq("dev.zio" %% "zio-test" % "1.0.8" % "test",
  "dev.zio" %% "zio-test-sbt" % "1.0.8" % "test"
)
testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))