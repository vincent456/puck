package puck

import java.awt.event.WindowEvent

import puck.actions.Choose
import puck.graph.{ConcreteNode, DependencyGraph, SResult}
import puck.graph.transformations.{Recording, TransformationRules}
import puck.jastadd.ExtendJGraphUtils.dotHelper
import puck.search._
import puck.util.LoggedEither
import puck.graph.DependencyGraph.ConstraintsOps
import puck.graph.constraints.ConstraintsMaps
import puck.jastadd.ExtendJGraphUtils.Rules
import puck.graph.constraints.search.{BlindControl, ControlWithHeuristic, SResultEvaluator}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import scalaz.\/-

/**
  * Created by cedric on 26/04/2016.
  */
object TestUtils {
  def showSuccess(ss : SearchState[(DependencyGraph, Int)]) = {
    val LoggedEither(_, \/-((g, _))) = ss.loggedResult
    Quick.frame(g, "G")
  }

  def showEngineSuccesses(engine : SearchEngine[(DependencyGraph, Int)]) =
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

  type ControleBuilder = (TransformationRules, DependencyGraph, ConstraintsMaps, ConcreteNode) => SearchControl[SResult]
  type StrategyBuilder = () => SearchStrategy[SResult]

  def solveAll(graph : DependencyGraph, constraints : ConstraintsMaps,
               controlBuilder : ControleBuilder,
               strategyBuilder : StrategyBuilder,
               previouslyRemainingViolation : Int = -1) : Option[DependencyGraph] =
    (graph, constraints).violations() match {
      case Seq() => Some(graph)
      case s @ (hd +: _) if s.size != previouslyRemainingViolation =>
        println(s.size + " remaining violations")
        val target = hd.target

        val searchControlStrategy =
          new BlindControl(
            Rules,
            graph, constraints, graph getConcreteNode target)

        val engine =
          new SearchEngine(
            strategyBuilder(),
            searchControlStrategy,
            Some(1)/*,
              evaluator = Some(SResultEvaluator.equalityByMapping(Metrics.nameSpaceCoupling))*/)

        engine.explore()

        if(engine.successes.isEmpty) None
        else{
          val LoggedEither(_, \/-((g, _))) = engine.successes.head.loggedResult

          solveAll(removeVirtualNodes(graph, g, Some(constraints)), constraints, controlBuilder, strategyBuilder, s.size)
        }
      case _ => Some(graph)
    }

  def solveAllBlindBFS(graph : DependencyGraph, constraints : ConstraintsMaps) : Option[DependencyGraph] =
    solveAll(graph, constraints, new BlindControl(_,_,_,_), () => new BreadthFirstSearchStrategy[(DependencyGraph, Int)])

  def solveAllBlindAStar(graph : DependencyGraph, constraints : ConstraintsMaps) : Option[DependencyGraph] =
    solveAll(graph, constraints, new BlindControl(_,_,_,_), () => new AStarSearchStrategy[(DependencyGraph, Int)] (SResultEvaluator.equalityByMapping(_ => 1)))

  def solveAllHeuristicBFS(graph : DependencyGraph, constraints : ConstraintsMaps) : Option[DependencyGraph] =
    solveAll(graph, constraints, new ControlWithHeuristic(_,_,_,_), () => new BreadthFirstSearchStrategy[(DependencyGraph, Int)])

  def solveAllHeuristicAStar(graph : DependencyGraph, constraints : ConstraintsMaps) : Option[DependencyGraph] =
    solveAll(graph, constraints, new ControlWithHeuristic(_,_,_,_), () => new AStarSearchStrategy[(DependencyGraph, Int)] (SResultEvaluator.equalityByMapping(_ => 1)))

}
