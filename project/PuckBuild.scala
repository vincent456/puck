
import java.io.FileWriter
import java.text.SimpleDateFormat
import java.util.Calendar

import com.typesafe.sbt.packager.archetypes.JavaAppPackaging
import com.typesafe.sbt.SbtNativePackager.Universal
import org.apache.commons.io.FileUtils
import sbt._
import Keys._

object PuckBuild extends Build {

  implicit class ProjectSettingsOp(p : Project){
    def settingsSeq(ss : Seq[Setting[_]]) : Project = p.settings(ss:_*)
  }

  val classPathFileName = settingKey[String]("Location of generated classpath script")

  val printClassPathFile = taskKey[File]("create a file containing the fullclass path")
  val jarSources = taskKey[File]("create a jar containing source files")
  val srcJarFileName = settingKey[String]("name of the jar containing source files")

  def commonSettings(module: String) : Seq[Setting[_]] = Seq(
    organization := "fr.lip6",
    name := s"puck-$module",
    version := "1.1",
    scalaVersion := "2.11.7",
    sbtVersion := "0.13.11",
    classPathFileName := "CLASSPATH",
    srcJarFileName := s"${organization.value}.${name.value}-${version.value}-src.jar",
    printClassPathFile := {

      val f = baseDirectory.value / "target" / classPathFileName.value

      val writter = new FileWriter(f)
      val fcp = (fullClasspath in Test).value.map(_.data.absolutePath)
      //writter.write(fcp.mkString("CLASSPATH=", ":", ""))
      // fish style :
      writter.write(fcp.mkString("set CLASSPATH ", ":", ""))
      writter.close()
      f
    },

    resolvers += Resolver.url("Typesafe Releases",
          url("https://repo.typesafe.com/typesafe/releases/"))(Resolver.ivyStylePatterns),

    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.1.2",
      "org.scalatest" %% "scalatest" % "2.2.1",
      "org.scala-sbt" %% "io" % "0.13.8"
    ),

    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",       // yes, this is 2 args
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:postfixOps",
      "-unchecked",
      "-Xlint",
      //"-Xfatal-warnings",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",        // N.B. doesn't work well with the ??? hole
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Xfuture",
      "-Ywarn-unused-import"  // 2.11 only
    ),


    printClassPathFile := {

      val f = baseDirectory.value / "target" / classPathFileName.value
      val writter = new FileWriter(f)
      val fcp = (fullClasspath in Compile).value.map(_.data.absolutePath)
      writter.write(fcp.mkString("set CLASSPATH ", ":", ""))
      writter.close()
      f
    },
    jarSources := {

      def listFileWithRelativePaths(roots : Seq[File]) : Seq[(File, String)] =
          roots.foldLeft (Seq[(File,String)]()) {
            (acc, dir) =>
              try {
                val fs = FileUtils.listFiles(dir, Array("scala","java"), true)
                import scala.collection.JavaConversions._
                fs.toSeq.foldLeft(acc) {
                  (acc0, f) =>
                    acc0 :+(f, f.relativeTo(dir).get.toString)

                }
              }
              catch {
                case _ : IllegalArgumentException => sys.error(dir + " is not a valid directory")
              }
          }


      val unmanagedDirs = (unmanagedSourceDirectories in Compile).value
      val managedDirs = (managedSourceDirectories in Compile).value

      val jarFile = baseDirectory.value / "target" / srcJarFileName.value
      val dirs = unmanagedDirs ++ managedDirs filter (_.isDirectory)

      IO.jar(listFileWithRelativePaths(dirs), jarFile, new java.util.jar.Manifest())
      jarFile
    }
  )


  lazy val root = (project in file(".")
    aggregate (puckCore, puckGui, puckJava, puckJrrt/*, puckScala*/)
  )

  val puckCore : Project = (project
    settingsSeq commonSettings("core")

    settingsSeq Seq[Setting[_]] {
        libraryDependencies +=
          "org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4"
    }
  )

  val puckGui = ( project
    settingsSeq commonSettings("gui")

    settingsSeq Seq[Setting[_]] {
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-swing" % "2.11.0-M7",
        "org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4",
        "org.apache.xmlgraphics" % "xmlgraphics-commons" % "2.0.1",
        "org.apache.xmlgraphics" % "batik-util" % "1.8",
        "org.apache.xmlgraphics" % "batik-svg-dom" % "1.8",
        "org.apache.xmlgraphics" % "batik-swing" % "1.8",
        "org.apache.xmlgraphics" % "batik-svggen" % "1.8" //for begugging purposes
      )
    }
    dependsOn (puckCore % "compile->compile")
  )

  val puckJava : Project = (project
    settingsSeq commonSettings("java")
    dependsOn (puckCore % "test->test;compile->compile")
   )


  val extendjUrl = "https://bitbucket.org/extendj/extendj.git"

  val extendjRef : ProjectReference = RootProject(uri(extendjUrl))
  //ProjectRef( uri(extendjUrl), "extendj")
  //val extendjPath = settingKey[ClasspathDependency]("extendjPath")



  val timeFormat = new SimpleDateFormat("yyyy-MM-dd-HHmmss")

  import com.typesafe.sbt.packager.Keys._
  val puckJrrt : Project = (project
    settingsSeq commonSettings("jrrt")

    dependsOn extendjRef
    settingsSeq PuckJrrtBuild.settings(extendjRef)

    enablePlugins JavaAppPackaging

    dependsOn (puckJava % "test->test;compile->compile")
    dependsOn (puckGui % "compile->compile")
    settings(packageName in Universal := s"puck-distrib-${timeFormat.format(Calendar.getInstance().getTime)}" )

    //pack sources with the binaries
//    settings (mappings in Universal ++=
//      Seq((jarSources in puckCore).value -> ("lib/" + (srcJarFileName in puckCore).value),
//        (jarSources in puckGui).value -> ("lib/" + (srcJarFileName in puckGui).value),
//        jarSources.value -> ("lib/" + srcJarFileName.value)
//
//      ))

    )

//  val puckScala : Project = (project
//    settingsSeq commonSettings("scala")
//
//    settingsSeq Seq[Setting[_]] {
//      libraryDependencies ++= Seq(
//        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
//        //"org.scala-lang" % "scala-library" % scalaVersion.value,
//        "org.scala-lang" % "scala-reflect" % scalaVersion.value
//      )
//    }
//
//    settingsSeq PuckScalaBuild.settings
//
//    dependsOn (puckGui % "compile->compile")
//  )

}
