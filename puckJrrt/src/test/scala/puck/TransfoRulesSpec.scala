package puck

import org.scalatest.FeatureSpec
import puck.graph.comparison.Mapping
import puck.graph.DependencyGraph
import puck.javaGraph.ScenarioFactory

/**
  * Created by lorilan on 5/5/16.
  */
class TransfoRulesSpec
  extends FeatureSpec
    with LoggedEitherValues{



  implicit def scenarioOfString(code : String) : ScenarioFactory =
    new ScenarioFactory(code)

  implicit def scenarioOfStrings(codes : Seq[String]) : ScenarioFactory =
    new ScenarioFactory(codes:_*)


  def compareWithExpectedAndGenerated(scenario : ScenarioFactory,
                                      transfo : ScenarioFactory => DependencyGraph,
                                      expectedResult: ScenarioFactory) : Unit = {

    def print(g : DependencyGraph, name : String) : Unit = {
      import puck.graph.ShowDG._
      println(s"graph $name")
//      (g, g.nodesIndex).println
      (g, g.edges).println
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

    def assertEqualityOrAssertFailureWithClue(g: DependencyGraph, other : DependencyGraph, otherName : String) = {
      val clue = s"graph produced by transfo != $otherName"
      try assert(Mapping.equals(g, other), clue)
      catch {
        case t : Throwable =>
//          println(clue)
//          t.printStackTrace()
          assert(false, s"$clue : ${t.getMessage}")
      }
    }

    assertEqualityOrAssertFailureWithClue(g, expectedResult.graph, "expected")

    someGenerated match {
      case Left(generated) =>
        assertEqualityOrAssertFailureWithClue(g, generated.graph, "generated")
      case Right(t) => t.printStackTrace()
        assert(false, t.getMessage)
    }
  }

}
