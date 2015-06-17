
import java.io.FileWriter

import com.typesafe.sbt.packager.archetypes.JavaAppPackaging
import sbt._
import Keys._

object PuckBuild extends Build {


  val classPathFileName = settingKey[String]("Location of generated classpath script")

  val printClassPathFile = taskKey[File]("create a file containing the fullclass path")

  def commonSettings(module: String) = Seq[Setting[_]](
    organization := "fr.lip6",
    name := s"puck-$module",
    version := "1.0-SNAPSHOT",
    scalaVersion := "2.11.6",
    sbtVersion := "0.13.8",
    classPathFileName := "CLASSPATH",

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
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.1.2",
      "org.scalatest" %% "scalatest" % "2.2.1"
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
    }
  )





  lazy val root = (project in file(".")
    settings commonSettings("root")
    dependsOn (puckCore % "test->test;compile->compile",
    puckJava % "test->test;compile->compile",
    puckScala % "test->test;compile->compile")
    aggregate (puckCore, puckJava, puckScala)
    )

  val puckCore = (project
    settings commonSettings("core")

    settings Seq[Setting[_]] {
        libraryDependencies ++= Seq(
          "org.scala-lang" % "scala-swing" % "2.11.0-M7",
          "org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4",
          //      "org.scala-lang" % "scala-compiler" % "2.11.6",
          //      "org.scala-lang" % "scala-reflect" % "2.11.6",
          "org.apache.xmlgraphics" % "batik-util" % "1.7",
          "org.apache.xmlgraphics" % "batik-svg-dom" % "1.7",
          "org.apache.xmlgraphics" % "batik-swing" % "1.7",
          "org.apache.xmlgraphics" % "batik-svggen" % "1.7" //for begugging purposes
        )
    }
  )

  val puckJava = (project
    settings commonSettings("java")
    settings PuckJavaBuild.settings
    enablePlugins JavaAppPackaging
    //dependsOn (puckCore % "test->test;compile->compile")
    dependsOn (puckCore % "compile->compile")

    )

  val puckScala = (project
    settings commonSettings("scala")

    settings Seq[Setting[_]] {
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-compiler" % "2.11.6",
        "org.scala-lang" % "scala-reflect" % "2.11.6"
      )
    }

    settings PuckScalaBuild.settings

    dependsOn (puckCore % "compile->compile")
  )

}