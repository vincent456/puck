package puck

import org.scalatest.FeatureSpec
import puck.graph.comparison.Mapping
import puck.graph.{DependencyGraph, ShowDG}
import puck.javaGraph.ScenarioFactory

/**
  * Created by lorilan on 5/5/16.
  */
class TransfoRulesSpec
  extends FeatureSpec
  with LoggedEitherValues{

  def compareWithExpectedAndGenerated(initialCode : String,
                                      transfo : ScenarioFactory => DependencyGraph,
                                      expectedResultCode: String) : Unit = puck.ignore {
    new ScenarioFactory(initialCode) {


      val g : DependencyGraph = transfo(this)

      import ShowDG._
      println("graph initial")
      (graph, graph.edges).println
      println("graph post transfo")
      (g, g.edges).println

      val expectedResult = new ScenarioFactory(expectedResultCode)
      println("graph expected")
      (expectedResult.graph, expectedResult.graph.edges).println
      assert(Mapping.equals(g, expectedResult.graph), "graph produced by transfo != expected")

      val generated = applyChangeAndMakeExample(g, Settings.outDir)
      assert(Mapping.equals(g, generated.graph), "graph produced by transfo != generated")
    }
  }

}
