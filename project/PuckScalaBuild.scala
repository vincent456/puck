import sbt._, Keys._

import scalaz._, Scalaz._

object PuckScalaBuild {

  val jarName = settingKey[File]("Jrrt build directory")
  val makejar = taskKey[File]("create the plugin jar")

  def settings = Seq[Setting[_]](
      jarName := baseDirectory.value / "target" / "puck-plugin.jar",
      makejar := {
        val classDir = (classDirectory in Compile).value
        val resDir = (resourceDirectory in Compile).value
        val libDir = (unmanagedBase in Compile).value
        val outFile = jarName.value

        val binFiles = (PathFinder(classDir) ** "*.class").get.toList
        val xmlFile = resDir / "scalac-plugin.xml"
        val libFiles = (PathFinder(libDir) ** "*.jar").get.toList

        val sbin = binFiles map (IO.relativize(classDir, _))
        val sxml = IO.relativize(resDir, xmlFile)
        val slibs = libFiles map (IO.relativize(libDir, _))

        val files = (sbin.sequence, sxml, slibs.sequence) match {
          case (Some(bin), Some(xml), Some(libs)) =>
            (xmlFile :: libFiles ::: binFiles) zip (xml :: libs ::: bin)
          case _ => sys.error(s"files not found")
        }
        IO.jar(files, outFile, new java.util.jar.Manifest())
        outFile
      }
    )
}
