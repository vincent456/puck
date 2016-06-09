package puck.javaGraph


import java.io.File

import puck.{GraphStack, Quick, Settings}
import puck.TestUtils._
import puck.graph.constraints.search.DecoratedGraphEvaluator
import puck.graph.transformations.{MileStone, Recording}
import puck.graph.{ConcreteNode, _}
import puck.gui.PuckControl
import puck.search.{AStarSearchStrategy, AStarSearchStrategyGraphDisplay}
import puck.jastadd.ExtendJGraphUtils.dotHelper
import puck.util.PuckSystemLogger
import puck.graph.io.DotPrinter

import scala.swing.Publisher
import scalaz.\/-


/**
  * Created by Mikal on 09/06/2016.
  */

object NanoTestSearch {
  val path = getClass.getResource("/nanoPersonne/").getPath

  val outDir = Settings.tmpDir + File.separator + "DG-Imgs"

  def main(args : Array[String]) : Unit = {
    val scenario = new ScenarioFactory(
      s"$path/nano/Personne.java",
      s"$path/nano/Client.java")


    val constraints = scenario.parseConstraints(s"$path/decouple.wld")

    //      val res = solveAll_targeted(graph, constraints, blindControlBuilder,
    //        () => new AStarSearchStrategy[(DependencyGraph, Int)](SResultEvaluator.equalityByMapping(_.numNodes)),
    //        Some(100),Some(5))


    val fitness1: DependencyGraph => Double =
      Metrics.fitness1(_, constraints, 1, 1)

    val evaluator = DecoratedGraphEvaluator.equalityByMapping[Option[ConcreteNode]](fitness1)
    val strategy = new AStarSearchStrategyGraphDisplay[Option[ConcreteNode]](
      evaluator, Some(constraints),
      10, 1000, outDir)

    implicit val ordering = AStarSearchStrategy.ordering(evaluator)

    val res = solveAllBlind(scenario.graph, constraints, strategy, Some(1))

    if (res.isEmpty) println("no results")
    else {
      println(res.size + " result(s)")
      res foreach {
        ss =>
          val fit = ordering.evaluateWithDepthPenalty(ss)
          strategy.printSuccessState("result#" + fit, ss)
          val \/-(dg) = ss.loggedResult.value
          val result = outDir + File.separator +"result#" + fit + ".pck"
          Recording.write(result,
            scenario.fullName2id, dg.graph)
          val s = new ScenarioFactory(
            s"$path/nano/Personne.java",
            s"$path/nano/Client.java")
          val r = Recording.load(s"$result", s.fullName2id)(s.logger)
          import Recording.RecordingOps
          val resdir = new File( Settings.tmpDir + File.separator+"testPuck"+fit)
          val sf = s.applyChangeAndMakeExample(r.redo(s.graph),resdir )
          Quick.svg(sf.graph, resdir + File.separator + "nano.svg", Some(constraints))

      }
    }
  }
}
