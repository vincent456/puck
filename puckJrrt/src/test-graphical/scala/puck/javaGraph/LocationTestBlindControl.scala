package puck.javaGraph

import puck.graph.constraints.search._
import puck.{AcceptanceSpec, Quick, Settings}
import puck.jastadd.ExtendJGraphUtils.dotHelper
import puck.TestUtils._
import puck.graph.{DependencyGraph, ShowDG}
import puck.graph.comparison.Mapping
import puck.search.{AStarSearchStrategy, BreadthFirstSearchStrategy}
import puck.util.LoggedEither
import puck.graph.DependencyGraph.ConstraintsOps

import scalaz.\/-


object LocationSolveAll {
  val path = getClass.getResource("/miniComposite").getPath

  def main(args : Array[String]) : Unit =
    new ScenarioFactory(
      s"$path/location/Location.java",
      s"$path/location/Velo.java") {

      val constraints = parseConstraints(s"$path/decouple.wld")

      val res = solveAllKeepVN(graph, constraints, blindControlBuilder,
        () => new AStarSearchStrategy[(DependencyGraph, Int)](SResultEvaluator.equalityByMapping(_.numNodes)),
        Some(1),Some(100))

      if(res.isEmpty) println("no results")
      else res foreach {
        case LoggedEither(_, \/-(g)) =>
          Quick.dot(g, Settings.tmpDir + "solved-blind_bfs", Some(constraints))
          Quick.frame(g, "Blind BFS", scm = Some(constraints))
      }



    }
}

/**
  * Created by cedric on 02/05/2016.
  */
object LocationTestBlindControl {

  val path = getClass.getResource("/miniComposite").getPath

  val bfsScenario =
    new ScenarioFactory(
      s"$path/location/Location.java",
      s"$path/location/Velo.java") {

      val constraints = parseConstraints(s"$path/decouple.wld")


      val res = solveAllBlindBFS(graph, constraints)
      res match {
        case None => println("no results")
        case Some(g) => Quick.dot(g, Settings.tmpDir + "solved-blind_bfs", Some(constraints))
      }

    }

  val aStarScenario = new ScenarioFactory(
    s"$path/location/Location.java",
    s"$path/location/Velo.java") {

    val constraints = parseConstraints(s"$path/decouple.wld")


    val res = solveAllBlindAStar(graph, constraints)

    res match {
      case None => println("no results")
      case Some(g) => Quick.dot(g, Settings.tmpDir + "solved-blind_aStar", Some(constraints))
    }

  }


}

import LocationTestBlindControl._

class LocationTestBlindControl
  extends AcceptanceSpec {

  scenario("location 1er test") {

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
