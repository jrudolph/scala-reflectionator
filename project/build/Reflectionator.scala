import sbt._

class ReflectionatorProject(info: ProjectInfo) extends DefaultProject(info) {
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"

  val specs = "org.scala-tools.testing" % "specs_2.8.0.RC2" % "1.6.5-SNAPSHOT" % "test"
}
