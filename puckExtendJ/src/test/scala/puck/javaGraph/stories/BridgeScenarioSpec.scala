package puck.javaGraph.stories

import org.scalatest.FeatureSpec
import puck.Settings._
import puck.graph.comparison.Mapping

/**
  * Created by Loïc Girault on 20/07/16.
  */
class BridgeScenarioSpec extends FeatureSpec {

  scenario("bridge ``manual'' refactoring"){
    val bs = BridgeScenario()

    //import puck.TestUtils.printMetrics

    //puck.Quick.dot(bs.graph, "/tmp/bridge.dot")(ExtendJGraphUtils.dotHelper)

    val recompiledEx = bs.applyChangeAndMakeExample(bs.gFinal, outDir)

    //    println("original")
    //    printMetrics(bs.graph)
    //    println("transformed")
    //    printMetrics(bs.gFinal)

    assert( Mapping.equals(bs.gFinal, recompiledEx.graph) )

  }
}
