
import com.github.retronym.SbtOneJar._

//https://github.com/retronym/sbt-onejar
oneJarSettings

name := "PuckConstraintSolver"

version := "1.0"

scalaVersion := "2.11.2"

mainClass in Compile := Some("puck.Front")

jrrtHome := baseDirectory.value / "jrrt"

jrrtReadOnly := jrrtHome.value / "jrrt-read-only"

java14frontend := jrrtHome.value / "Java1.4Frontend"

java15frontend := jrrtHome.value / "Java1.5Frontend"

controlFlowGraph := jrrtHome.value / "ControlFlowGraph"

jastaddSrcDir := baseDirectory.value / "src" / "main" / "jrag"

jastaddOutDir := sourceManaged.value / "main"

java15comply := true

libraryDependencies ++= Seq( 
	"org.scala-lang" % "scala-swing" % "2.11.0-M7",
	"org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4")

(libraryDependencies in Test) += "org.scalatest" % "scalatest_2.11" % "2.2.1"

(sourceGenerators in Compile) ++= Seq(parser.taskValue, scanner.taskValue, ast.taskValue)

(sourceGenerators in Test) ++= Seq(parser.taskValue, scanner.taskValue, ast.taskValue)

cleanFiles += jastaddOutDir.value

scalacOptions ++=Seq("-deprecation","-unchecked","-feature")