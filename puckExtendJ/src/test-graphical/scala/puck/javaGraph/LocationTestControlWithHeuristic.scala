package puck.javaGraph

import puck.{AcceptanceSpec, Quick, Settings}
import puck.jastadd.ExtendJGraphUtils.dotHelper
import puck.TestUtils._
import puck.graph.comparison.Mapping

/**
  * Created by cedric on 02/05/2016.
  */
object LocationTestControlWithHeuristic {

  val path = getClass.getResource("/miniComposite").getPath

  val bfsScenario =
    new ScenarioFactory(
      s"$path/location/Location.java",
      s"$path/location/Velo.java") {

      val constraints = parseConstraints(s"$path/decouple.wld")

      val res = solveAllHeuristicBFS(graph, constraints)


      res match {
        case None => println("no results")
        case Some(g) => Quick.dot(g, Settings.tmpDir + "solved-blind_bfs", Some(constraints))
      }

    }

  val aStarScenario = new ScenarioFactory(
    s"$path/location/Location.java",
    s"$path/location/Velo.java") {

    val constraints = parseConstraints(s"$path/decouple.wld")


    val res = solveAllHeuristicAStar(graph, constraints)

    res match {
      case None => println("no results")
      case Some(g) => Quick.dot(g, Settings.tmpDir + "solved-heuristic_aStar", Some(constraints))
    }

  }


}

import LocationTestControlWithHeuristic._

class LocationTestControlWithHeuristic
  extends AcceptanceSpec {

  scenario("location 2eme test") {

    (bfsScenario.res, aStarScenario.res) match {
      case (Some(g), Some(g2)) if Mapping.equals(g, g2) =>
        bfsScenario.applyChangeAndMakeExample(g, Settings.tmpDir+"out/heuristic_common")

        Quick.frame(g, "Heuristic BFS & A Star", scm = Some(bfsScenario.constraints))
        assert(true)
      case _ =>
        bfsScenario.res foreach { g =>
          Quick.frame(g, "Heuristic BFS", scm = Some(bfsScenario.constraints))
          bfsScenario.applyChangeAndMakeExample(g, Settings.tmpDir+"out/heuristic_bfs")
        }

        aStarScenario.res foreach { g2 =>
          Quick.frame(g2, "Heuristic A Star", scm = Some(aStarScenario.constraints))
          aStarScenario.applyChangeAndMakeExample(g2, Settings.tmpDir + "out/heuristic_astar")
        }
        assert(false)
    }

  }

}
