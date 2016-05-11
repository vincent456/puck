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
                                      expectedResultCode: String) : Unit =
    compareScenarioWithExpectedAndGenerated(new ScenarioFactory(initialCode),
      transfo, new ScenarioFactory(expectedResultCode))

  def compareScenarioWithExpectedAndGenerated(scenario : ScenarioFactory,
                                              transfo : ScenarioFactory => DependencyGraph,
                                              expectedResult: ScenarioFactory) : Unit = {
    val g : DependencyGraph = transfo(scenario)


    import ShowDG._
    println("graph initial")
    (scenario.graph, scenario.graph.nodesIndex).println
    //(scenario.graph, scenario.graph.edges).println
    println("graph post transfo")
    (g, g.nodesIndex).println
    //(g, g.edges).println
    println("graph expected")
    (expectedResult.graph, expectedResult.graph.nodesIndex).println
    //(expectedResult.graph, expectedResult.graph.edges).println

    assert(Mapping.equals(g, expectedResult.graph), "graph produced by transfo != expected")

    val generated = scenario.applyChangeAndMakeExample(g, Settings.outDir)
    assert(Mapping.equals(g, generated.graph), "graph produced by transfo != generated")

  }

}
