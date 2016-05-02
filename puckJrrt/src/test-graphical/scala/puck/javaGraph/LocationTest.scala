package puck.javaGraph

import puck.{AcceptanceSpec, Quick}
import puck.jastadd.ExtendJGraphUtils.dotHelper
import puck.TestUtils._

/**
  * Created by cedric on 02/05/2016.
  */
object LocationTest {
  val path = getClass.getResource("/miniComposite").getPath
}

import LocationTest.path
class LocationTest
  extends AcceptanceSpec {

  scenario("location 1er test"){

    val _ = new ScenarioFactory(
      s"$path/location/Location.java",
      s"$path/location/Velo.java"){

      val constraints = parseConstraints(s"$path/decouple.wld")

      solveAllBlindBFS(graph, constraints) match {
        case None => println("no results")
        case Some(g) => Quick.dot(g, "/tmp/solved-blind_bfs", Some(constraints))
          Quick.frame(g, scm = Some(constraints))
      }
      solveAllBlindAStar(graph, constraints) match {
        case None => println("no results")
        case Some(g) => Quick.dot(g, "/tmp/solved-blind_astar", Some(constraints))
          Quick.frame(g, scm = Some(constraints))
      }

    }

  }



}
