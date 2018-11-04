import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "kurtlogan",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "hangman",
    libraryDependencies += "org.scalaz" %% "scalaz-zio" % "0.3.1",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.26",
    libraryDependencies += scalaTest % Test
  )
