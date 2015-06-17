package puck

object Settings {
  val envVarName = "puck_project_dir"
  val projectPath = sys.env get envVarName match {
    case Some (d) => d
    case None => throw new PuckError(s"$envVarName undefined : please define an environment named $envVarName variable with the project path")
  }
  val tmpDir = "/tmp/"
  val testPath = projectPath + "puckJava/src/test/"
  val testExamplesPath = testPath + "resources/examples/"
}
