package puck.javaGraph.search

import java.io.File

import puck.TestUtils._
import puck.graph.constraints.search.DecoratedGraphEvaluator
import puck.graph.{ConcreteNode, _}
import puck.javaGraph.ScenarioFactory
import puck.Settings
import puck.search.AStarSearchOrdering
import puck.jastadd.ExtendJGraphUtils.dotHelper

import scala.language.reflectiveCalls

/**
  * Created by cedric on 18/05/2016.
  */

object PersonneTestSearch {
  val path = getClass.getResource("/projetPersonne/").getPath

  val outDir = SearchTest.outDir + File.separator + "DG-Imgs"

  def main(args : Array[String]) : Unit = {
    val filePaths = Seq(
      s"$path/personne/Personne.java",
      s"$path/personne/Client.java")
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

    SearchTest.printResult(res, ordering, scenario.fullName2id, filePaths:_*)

  }
}
