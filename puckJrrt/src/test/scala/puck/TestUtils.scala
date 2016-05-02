package puck

import puck.actions.Choose
import puck.graph.DependencyGraph
import puck.graph.transformations.Recording
import puck.jastadd.ExtendJGraphUtils.dotHelper
import puck.search.{BreadthFirstSearchStrategy, SearchEngine, SearchState}
import puck.util.LoggedEither
import puck.graph.DependencyGraph.ConstraintsOps
import puck.graph.constraints.ConstraintsMaps
import puck.graph.constraints.search.BlindControl
import puck.jastadd.ExtendJGraphUtils.transformationRules

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

  def removeVirtualNodes(gWithoutVirtualNodes : DependencyGraph,
                         gWithVirtualNodes : DependencyGraph) : DependencyGraph = {
    var g0 = gWithVirtualNodes
    while (g0.virtualNodes.nonEmpty) {
      Quick.frame(g0, "G")
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
    }
    g0
  }

  def solveAll(graph : DependencyGraph, constraints : ConstraintsMaps, previouslyRemainingViolation : Int = -1) : Option[DependencyGraph] =
    (graph, constraints).violations() match {
      case Seq() => Some(graph)
      case s @ (hd +: _) if s.size != previouslyRemainingViolation =>
        println(s.size + " remaining violations")
        val target = hd.target

        val searchControlStrategy =
          new BlindControl(
            transformationRules,
            graph, constraints, graph getConcreteNode target)

        val engine =
          new SearchEngine(
            new BreadthFirstSearchStrategy[(DependencyGraph, Int)],
            searchControlStrategy,
            Some(1)/*,
              evaluator = Some(SResultEvaluator.equalityByMapping(Metrics.nameSpaceCoupling))*/)

        engine.explore()

        if(engine.successes.isEmpty) None
        else{
          val LoggedEither(_, \/-((g, _))) = engine.successes.head.loggedResult

          solveAll(removeVirtualNodes(graph, g), constraints, s.size)
        }
      case _ => Some(graph)
    }
}
