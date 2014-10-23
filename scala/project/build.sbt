
libraryDependencies ++= Seq(
	"net.sf.beaver" % "beaver-cc" % "0.9.11",
	"net.sf.beaver" % "beaver-rt" % "0.9.11",
	"de.jflex" % "jflex" % "1.6.0",
  "commons-lang" % "commons-lang" % "2.6" // for oneJar plugin
)

scalacOptions += "-deprecation"