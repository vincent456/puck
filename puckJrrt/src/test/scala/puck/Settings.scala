package puck

import java.io.File

object Settings {
  val envVarName = "puck_project_dir"

  val projectPath =
    sys.env get envVarName match {
    case Some (d) => d
    case None => throw new PuckError(s"$envVarName undefined : please define an environment variable named $envVarName  with the project path")
  }
  val tmpDir = "/tmp/"
  val testPath = projectPath + "puckJava/src/test/"
  val testExamplesPath = testPath + "resources/examples/"

  val outDir = new File(Settings.tmpDir + "testPuck")
}
