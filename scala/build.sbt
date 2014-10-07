name := "Puck"

moduleName := "ConstraintSolver"

version := "1.0"

scalaVersion := "2.11.2"

mainClass in Compile := Some("puck.Front")

libraryDependencies ++= Seq( 
	"org.scala-lang" % "scala-swing" % "2.11.0-M7",
	"org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4",
  "org.scalatest" % "scalatest_2.11" % "2.2.1"
	)