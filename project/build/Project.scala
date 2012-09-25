import sbt._

class MyProject(info: ProjectInfo) extends DefaultProject(info) with com.github.retronym.OneJarProject {
  override def mainClass = Some("gausel.ui.Gausel")
}
