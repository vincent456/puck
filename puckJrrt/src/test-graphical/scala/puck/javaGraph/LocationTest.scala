package puck.javaGraph

import java.io.File

import puck.{AcceptanceSpec, Quick, Settings}
import puck.jastadd.ExtendJGraphUtils.dotHelper
import puck.TestUtils._
import puck.graph.DependencyGraph
import puck.graph.comparison.Mapping

/**
  * Created by cedric on 02/05/2016.
  */
object LocationTest {

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

import LocationTest._

class LocationTest
  extends AcceptanceSpec {

  scenario("location 1er test") {

    (bfsScenario.res, aStarScenario.res) match {
      case (Some(g), Some(g2)) if Mapping.equals(g, g2) =>

        Quick.frame(g, "Blind BFS & A Star", scm = Some(bfsScenario.constraints))
        assert(true)
      case _ =>
        bfsScenario.res foreach (Quick.frame(_, "Blind BFS", scm = Some(bfsScenario.constraints)))
        aStarScenario.res foreach (Quick.frame(_, "Blind A Star", scm = Some(aStarScenario.constraints)))
        assert(false)
    }

  }

}
