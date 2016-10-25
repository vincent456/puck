package puck.javaGraph.search

import java.io.File

import org.extendj.ExtendJGraphUtils.dotHelper
import puck.TestUtils._
import puck.graph.constraints.search.DecoratedGraphEvaluator
import puck.graph.{ConcreteNode, _}
import puck.javaGraph.ScenarioFactory
import puck.search.AStarSearchOrdering

/**
  * Created by cedric on 23/05/2016.
  */

object BridgeTestBlindControlSolveAll {
  val path = getClass.getResource("/bridge/hannemann_simplified/").getPath

  val outDir = SearchTest.outDir + File.separator + "DG-Imgs"

  def main(args : Array[String]) : Unit = {
    val filePaths = Seq(
      s"$path/screen/BridgeDemo.java",
      s"$path/screen/Screen.java")
    val scenario = new ScenarioFactory(filePaths:_*)

    val constraints = scenario.parseConstraintsFile(s"$path/decouple.wld")

    //      val res = solveAll_targeted(graph, constraints, blindControlBuilder,
    //        () => new AStarSearchStrategy[(DependencyGraph, Int)](SResultEvaluator.equalityByMapping(_.numNodes)),
    //        Some(100),Some(5))


    val fitness1: DependencyGraph => Double =
      Metrics.fitness1(_, constraints, 1, 1).toDouble

    val evaluator = DecoratedGraphEvaluator.equalityByMapping[Option[ConcreteNode]](fitness1)
    val strategy = new AStarSearchStrategyGraphDisplayOnly[Option[ConcreteNode]](
      evaluator, Some(constraints),
      10, 1000, outDir)

    implicit val ordering = new AStarSearchOrdering(evaluator)

    val res = solveAllBlind(scenario.graph, constraints,
      scenario.initialMutability, strategy, Some(1))

    SearchTest.printResult(res, ordering, scenario.fullName2id, constraints, filePaths:_*)
  }
}
