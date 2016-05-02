package puck.javaGraph

import puck.{AcceptanceSpec, QuickFrame}
import puck.jastadd.ExtendJGraphUtils
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

      QuickFrame(graph, "bob",ExtendJGraphUtils.dotHelper)
      solveAll(graph, constraints) match {
        case None => println("no results")
        case Some(g) => QuickFrame(g, "solved", ExtendJGraphUtils.dotHelper)
      }
    }

  }



}
