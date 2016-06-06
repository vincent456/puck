package puck.javaGraph


import java.io.File

import puck.TestUtils._
import puck.graph.constraints.search.DecoratedGraphEvaluator
import puck.graph.{ConcreteNode, _}
import puck.search.{AStarSearchStrategy, AStarSearchStrategyGraphDisplay}
import puck.jastadd.ExtendJGraphUtils.dotHelper

import scalaz.\/-


/**
  * Created by cedric on 18/05/2016.
  */

object LocationTestBlindControlSolveAll {
  val path = getClass.getResource("/miniComposite").getPath

  def main(args : Array[String]) : Unit =
    new ScenarioFactory(
      s"$path/location/Location.java",
      s"$path/location/Velo.java") {

      val constraints = parseConstraints(s"$path/decouple.wld")

      //      val res = solveAll_targeted(graph, constraints, blindControlBuilder,
      //        () => new AStarSearchStrategy[(DependencyGraph, Int)](SResultEvaluator.equalityByMapping(_.numNodes)),
      //        Some(100),Some(5))


      val fitness1 : DependencyGraph => Double =
        Metrics.fitness1(_, constraints, 1, 1, 2)

      val evaluator = DecoratedGraphEvaluator.equalityByMapping[Option[ConcreteNode]](fitness1)
      val strategy =new AStarSearchStrategyGraphDisplay[Option[ConcreteNode]](
        evaluator, Some(constraints),
        10, 1000, "/tmp/DG-Imgs")

      implicit val ordering = AStarSearchStrategy.ordering(evaluator)

      val res = solveAllBlind(graph, constraints, strategy, Some(1))

      if(res.isEmpty) println("no results")
      else {
        println(res.size + " result(s)")
        res foreach {
           ss =>
             val fit = ordering.evaluateWithDepthPenalty(ss)
              strategy.printSuccessState("result#" + fit, ss)
             val \/-(dg) = ss.loggedResult.value
             Recording.write( "/tmp/DG-Imgs/result#" + fit + ".pck", fullName2id, dg.graph)
           }

        val \/-(dg) = res.head.loggedResult.value
        applyChanges(dg.graph, new File("/tmp/puck-location"))
       }
    }
}
