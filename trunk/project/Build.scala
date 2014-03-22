import sbt._
import Keys._
import com.github.retronym.SbtOneJar.oneJarSettings

object GauselBuild extends Build {

  lazy val gausel = Project(
    id = "gausel", base = file("."), settings =
      Defaults.defaultSettings ++ Seq(
        organization := "standard",
        version := "1.1",
        name := "gausel",
        scalaVersion := "2.10.3",
        scalacOptions ++=
          Seq("-feature", "-deprecation","-unchecked", "-optimise","-encoding", "utf8"),
        exportJars := true
      ) ++ oneJarSettings
  )

  import sbt.Project.Initialize
}
