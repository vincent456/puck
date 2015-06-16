
libraryDependencies ++= Seq(
	"net.sf.beaver" % "beaver-cc" % "0.9.11",
	"net.sf.beaver" % "beaver-rt" % "0.9.11",
	"de.jflex" % "jflex" % "1.6.0",
	"org.scalaz" %% "scalaz-core" % "7.1.2")

scalacOptions ++=Seq("-deprecation", "-unchecked", "-feature", "-Xfatal-warnings" )