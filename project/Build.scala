import sbt._
import Keys._


object ProjectBuild extends Build {

  lazy val copyPom = TaskKey[Unit]("copy-pom")

  def copyPomTask(base: File) = copyPom <<= makePom map { pom =>
    IO.copyFile(pom, base / "pom.xml")
  }

  lazy val base = file(".").getCanonicalFile

  lazy val root = Project(
    id = "atari-st-tools",
    base = base,
    settings = Defaults.defaultSettings ++ Seq(copyPomTask(base))
  )
}

