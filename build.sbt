
enablePlugins(JavaAppPackaging)

name := "PuckConstraintSolver"

version := "1.0"

scalaVersion := "2.11.6"

sbtVersion := "0.13.8"

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

classPathFileName := "CLASSPATH"

//without this option, there is "cannot assign instance of scala.collection.immutable.List$SerializationProxy"
// Cast exception raised in RecordingSerializationSpec ...
fork := true

libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-swing" % "2.11.0-M7",
	"org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4",
	"org.scalaz" %% "scalaz-core" % "7.1.0",
  "org.scalatest" % "scalatest_2.11" % "2.2.1")


libraryDependencies ++= Seq(
  "org.apache.xmlgraphics" % "batik-util" % "1.7",
  "org.apache.xmlgraphics" % "batik-svg-dom" % "1.7",
  "org.apache.xmlgraphics" % "batik-swing" % "1.7",
  "org.apache.xmlgraphics" % "batik-svggen" % "1.7")//for beguggin purposes

//libraryDependencies += "org.scala-lang" % "scala-library-all" % "2.11.4" //before the modularize them

//(libraryDependencies in Test) += "org.scalatest" % "scalatest_2.11" % "2.2.1"

(sourceGenerators in Compile) ++= Seq(parser.taskValue, scanner.taskValue, ast.taskValue)

(sourceGenerators in Test) ++= Seq(parser.taskValue, scanner.taskValue, ast.taskValue)

cleanFiles += jastaddOutDir.value

parallelExecution in test := false  //cannot compile several program in parallel with jastadd

scalacOptions ++=Seq("-deprecation",
  "-encoding", "UTF-8",       // yes, this is 2 args
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  //"-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-dead-code",        // N.B. doesn't work well with the ??? hole
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Ywarn-unused-import")     // 2.11 only

//(scalacOptions in Compile) += "-Ywarn-numeric-widen"
