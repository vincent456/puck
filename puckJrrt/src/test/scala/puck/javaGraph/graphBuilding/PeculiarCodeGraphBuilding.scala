package puck.javaGraph.graphBuilding

import java.io.File

import puck.config.Config.SingleFile
import puck.config.{Config, ConfigParser}
import puck.jastadd.JavaJastAddDG2AST
import puck.javaGraph.ScenarioFactory
import puck.util.{PuckFileLogger, PuckSystemLogger}
import puck.{Settings, AcceptanceSpec, Project}

/**
  * Created by lorilan on 25/02/16.
  */
class PeculiarCodeGraphBuilding extends AcceptanceSpec {

  val graphBuildingExamplesPath = Settings.testExamplesPath + "/graphBuilding/"

  feature("fixed number of nodes") {

    //implicit val logger = new PuckSystemLogger(_ => true)
    implicit val logger = new PuckFileLogger(_ => true, new java.io.File("/tmp/debugLog"))

    scenario("freemind problem") {
      val cfg = Config.empty put (Config.Keys.srcs, List(SingleFile(s"$graphBuildingExamplesPath/complexHierarchy/View.java")))
      val p = new Project(cfg, JavaJastAddDG2AST)
      val s0 = p.loadGraph().asInstanceOf[JavaJastAddDG2AST]

      val n0Number = s0.initialGraph.nodes.size
      Range(0, 10).foreach { _ =>
        val s = p.loadGraph().asInstanceOf[JavaJastAddDG2AST]
        val nNumber = s.initialGraph.nodes.size

        assert(nNumber == n0Number)
      }
    }
  }
}
