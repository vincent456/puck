package puck

import java.awt.event.WindowEvent

import puck.actions.Choose
import puck.graph.Metrics._
import puck.graph.{ConcreteNode, DecoratedGraph, DependencyGraph, LoggedTG, NodeId, SResult}
import puck.graph.transformations.Recording
import puck.jastadd.ExtendJGraphUtils.dotHelper
import puck.search._
import puck.util.LoggedEither
import puck.graph.constraints.ConstraintsMaps
import puck.graph.constraints.search._
import puck.jastadd.ExtendJGraphUtils.{Rules, violationsKindPriority}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import scalaz.\/-

/**
  * Created by cedric on 26/04/2016.
  */
object TestUtils {
  def showSuccess[T](ss : SearchState[DecoratedGraph[T]]) = {
    val LoggedEither(_, \/-(dg)) = ss.loggedResult
    Quick.frame(dg.graph, "G")
  }

  def showEngineSuccesses[T](engine : SearchEngine[DecoratedGraph[T]]) =
    engine.successes foreach showSuccess

  def removeVirtualNodes(gWithoutVirtualNodes: DependencyGraph,
                         gWithVirtualNodes: DependencyGraph,
                         scm: Option[ConstraintsMaps] = None): DependencyGraph = {
    var g0 = gWithVirtualNodes
    while (g0.virtualNodes.nonEmpty) {
      val ff = Quick.frame(g0, "G", scm)
      val vn = g0.virtualNodes.head
      Choose("Concretize node",
        s"Select a concrete value for the virtual node $vn :",
        vn.potentialMatches.toSeq map g0.getConcreteNode) match {
        case None => ()
        case Some(cn) =>
          import Recording.RecordingOps
          val r2 = g0.recording.subRecordFromLastMilestone.concretize(vn.id, cn.id)
          g0 = r2 redo gWithoutVirtualNodes
      }
      ff onComplete {
        case Success(jframe) =>
          jframe.dispatchEvent(new WindowEvent(jframe, WindowEvent.WINDOW_CLOSING));
        case _ => println("failure")
      }
    }
    g0
  }

  def solveAllHeuristic(graph : DependencyGraph, constraints : ConstraintsMaps,
                        strategyBuilder : StrategyBuilder[SResult],
                        maxResultTotal : Option[Int]) : Seq[LoggedTG] = {
    val control = new ControlWithHeuristic(Rules, graph, constraints, violationsKindPriority)
    val engine = new SearchEngine(strategyBuilder(), control, maxResultTotal )
    engine.explore()
    engine.successes map (ss => ss.loggedResult map (_._1))
  }

  def solveAllBlind(graph : DependencyGraph, constraints : ConstraintsMaps,
                       strategy : SearchStrategy[DecoratedGraph[Option[ConcreteNode]]],
                       maxResult : Option[Int]) : Seq[SearchState[DecoratedGraph[Option[ConcreteNode]]]] = {
    val control = new BlindControl(Rules, graph, constraints, violationsKindPriority)
    val engine = new SearchEngine(strategy, control, maxResult)
    engine.explore()
    engine.successes
  }
  def solveAllBlindEval(graph : DependencyGraph, constraints : ConstraintsMaps,
                       strategy : SearchStrategy[DecoratedGraph[Option[ConcreteNode]]],
                           evaluator : Option[Evaluator [DecoratedGraph[Option[ConcreteNode]]]],
                       maxResult : Option[Int]) : Seq[SearchState[DecoratedGraph[Option[ConcreteNode]]]] = {
    val control = new BlindControl(Rules, graph, constraints, violationsKindPriority)
    val engine = new SearchEngine(strategy, control, maxResult, evaluator)
    engine.explore()
    engine.successes
  }

  implicit def toOption( s : Seq[LoggedTG]) : Option[DependencyGraph] = s match {
    case Nil => None
    case LoggedEither(_, \/-(g))  +: _ => Some(g)
  }

  def printMetrics(g : DependencyGraph) : Unit = {
    println(s"############################################")
    println(s"############################################")
    def p[T](name : String, metric : (DependencyGraph, NodeId) => T, avg : Seq[T] => Double) : Unit = {
      println(s"******\t$name\t******")
      val lcoms = apply_metric_on_types(metric, g, Seq("@primitive", "java"))
      println(lcoms mkString "\n")
      println("average = " + avg(lcoms.map(_._2)))
    }
    p("LCOM", LCOM, averageI)

    p("LCOM hs", LCOM_hs, averageD)

    p("LCOM4", LCOM4, averageI)

    p("CBO", CBO, averageI)

    println()
  }

}
