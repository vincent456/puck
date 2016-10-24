package puck.javaGraph

import puck.graph.constraints.search._
import puck.{AcceptanceSpec, Quick, Settings}
import puck.jastadd.ExtendJGraphUtils.dotHelper
import puck.TestUtils._
import puck.graph.{DependencyGraph, Metrics, ShowDG}
import puck.graph.comparison.Mapping
import puck.search.{AStarSearchStrategy, BreadthFirstSearchStrategy}
import puck.util.LoggedEither
import puck.graph.ConstraintsOps

import scalaz.\/-




/**
  * Created by cedric on 02/05/2016.
  */
object BridgeTestBlindControl {

  val path = getClass.getResource("/bridge/hannemann_simplified").getPath

  val bfsScenario =
    new ScenarioFactory(
      s"$path/screen/BridgeDemo.java",
      s"$path/screen/Screen.java") {

      val constraints = parseConstraints(s"$path/decouple.wld")


      val res = solveAllBlindBFS(graph, constraints)
      res match {
        case None => println("no results")
        case Some(g) => Quick.dot(g, Settings.tmpDir + "solved-blind_bfs", Some(constraints))
      }

    }

  val aStarScenario = new ScenarioFactory(
    s"$path/screen/BridgeDemo.java",
    s"$path/screen/Screen.java") {


    val constraints = parseConstraints(s"$path/decouple.wld")


    val res = solveAllBlindAStar(graph, constraints)

    res match {
      case None => println("no results")
      case Some(g) => Quick.dot(g, Settings.tmpDir + "solved-blind_aStar", Some(constraints))
    }

  }


}

import BridgeTestBlindControl._

class BridgeTestBlindControl
  extends AcceptanceSpec {

  scenario("Bridge Hannemann Simplified 1er test") {

    (bfsScenario.res, aStarScenario.res) match {
      case (Some(g), Some(g2)) if Mapping.equals(g, g2) =>
        //        bfsScenario.applyChangeAndMakeExample(g, Settings.tmpDir+"out/blind_common")

        Quick.frame(g, "Blind BFS & A Star", scm = Some(bfsScenario.constraints))
        assert(true)
      case _ =>
        bfsScenario.res foreach { g =>
          Quick.frame(g, "Blind BFS", scm = Some(bfsScenario.constraints))
          //          bfsScenario.applyChangeAndMakeExample(g, Settings.tmpDir+"out/blind_bfs")
        }

        aStarScenario.res foreach { g2 =>
          Quick.frame(g2, "Blind A Star", scm = Some(aStarScenario.constraints))
          //        aStarScenario.applyChangeAndMakeExample(g2, Settings.tmpDir + "out/blind_astar")
        }
        assert(false)
    }

  }

}
