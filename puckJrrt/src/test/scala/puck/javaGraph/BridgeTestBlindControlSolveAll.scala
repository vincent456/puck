package puck.javaGraph

import java.io.File

import puck.{Quick, Settings}
import puck.TestUtils._
import puck.graph.constraints.search.DecoratedGraphEvaluator
import puck.graph.{DependencyGraph, Metrics}
import puck.search.AStarSearchStrategyGraphDisplay
import puck.util.LoggedEither
import puck.jastadd.ExtendJGraphUtils.dotHelper

import scalaz.\/-

/**
  * Created by cedric on 23/05/2016.
  */
object BridgeTestBlindControlSolveAll {
  val path = getClass.getResource("/bridge/hannemann_simplified").getPath

  def main(args : Array[String]) : Unit =
    new ScenarioFactory(
      s"$path/screen/BridgeDemo.java",
      s"$path/screen/Screen.java") {


      val constraints = parseConstraints(s"$path/decouple.wld")

      //      val res = solveAll_targeted(graph, constraints, blindControlBuilder,
      //        () => new AStarSearchStrategy[(DependencyGraph, Int)](SResultEvaluator.equalityByMapping(_.numNodes)),
      //        Some(100),Some(5))


      val fitness1 : DependencyGraph => Double =
        Metrics.fitness1(_, constraints, 1, 1, 2)

      val res = solveAllBlind(graph, constraints,
        () => new AStarSearchStrategyGraphDisplay(DecoratedGraphEvaluator.equalityByMapping(fitness1),Some(constraints),
          10, 1000, "/tmp/"),
        Some(1))

      if(res.isEmpty) println("no results")
      else {
        println(res.size + " result(s)")
        //      res foreach {
        //        case LoggedEither(_, \/-(g)) =>
        //Quick.dot(g, Settings.tmpDir + "solved-blind_bfs", Some(constraints))
        //Quick.frame(g, "Blind BFS", scm = Some(constraints))
        //applyChanges(g, new File(Settings.tmpDir))
        //   }
      }
    }
}
