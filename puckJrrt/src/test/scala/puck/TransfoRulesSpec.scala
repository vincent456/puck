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

    def print(g : DependencyGraph, name : String) : Unit = {
//      import ShowDG._
//      println(s"graph $name")
////      (g, g.nodesIndex).println
//      (g, g.edges).println
    }


    print(scenario.graph, "initial")

    val g : DependencyGraph = transfo(scenario)

    print(g, "post transfo")

    print(expectedResult.graph, "expected")

    val someGenerated =
      try Left(scenario.applyChangeAndMakeExample(g, Settings.outDir))
      catch {
        case t : Throwable => Right(t)
      }

    assert(Mapping.equals(g, expectedResult.graph), "graph produced by transfo != expected")


    someGenerated match {
      case Left(generated) => assert(Mapping.equals(g, generated.graph), "graph produced by transfo != generated")
      case Right(t) => t.printStackTrace()
        assert(false, t.getMessage)
    }


  }

}
