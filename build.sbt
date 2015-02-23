
import com.github.retronym.SbtOneJar._

//https://github.com/retronym/sbt-onejar
oneJarSettings

name := "PuckConstraintSolver"

version := "1.0"

scalaVersion := "2.11.0"

sbtVersion := "0.13.5"

mainClass in Compile := Some("puck.Front")

//scalaSource -= baseDirectory.value / "test" / "resources"

jrrtHome := baseDirectory.value / "jrrt"

jrrtReadOnly := jrrtHome.value / "jrrt-read-only"

java14frontend := jrrtHome.value / "Java1.4Frontend"

java15frontend := jrrtHome.value / "Java1.5Frontend"

java16frontend := jrrtHome.value / "Java1.6Frontend"


controlFlowGraph := jrrtHome.value / "ControlFlowGraph"

jastaddSrcDir := baseDirectory.value / "src" / "main" / "jrag"

jastaddOutDir := sourceManaged.value / "main"

java15comply := true


libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-swing" % "2.11.0-M7",
	"org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4",
	"org.scalaz" %% "scalaz-core" % "7.1.0",
  "org.scalatest" % "scalatest_2.11" % "2.2.1")

//libraryDependencies += "org.scala-lang" % "scala-library-all" % "2.11.4" //before the modularize them

//(libraryDependencies in Test) += "org.scalatest" % "scalatest_2.11" % "2.2.1"

(sourceGenerators in Compile) ++= Seq(parser.taskValue, scanner.taskValue, ast.taskValue)

(sourceGenerators in Test) ++= Seq(parser.taskValue, scanner.taskValue, ast.taskValue)

cleanFiles += jastaddOutDir.value

scalacOptions ++=Seq("-deprecation",
  "-encoding", "UTF-8",       // yes, this is 2 args
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  //"-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",        // N.B. doesn't work well with the ??? hole
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Ywarn-unused-import")     // 2.11 only