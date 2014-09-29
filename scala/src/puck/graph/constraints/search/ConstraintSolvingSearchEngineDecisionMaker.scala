package puck.graph.constraints.search

import puck.graph.constraints.{AbstractionPolicy, DecisionMaker, NodeSet}
import puck.graph.NodeKind
import puck.search.{SearchState, SearchEngine}
import puck.util.Logger

import scala.collection.mutable

object ConstraintSolving {
  type NodeChoice[Kind <: NodeKind[Kind]] = ConstraintSolvingNodesChoice[Kind]
  type AbsChoice[Kind <: NodeKind[Kind]] = ConstraintSolvingAbstractionChoice[Kind]
  type FinalState[Kind <: NodeKind[Kind]] = SearchState[NodeChoice[Kind], NodeChoice[Kind]]
}

import ConstraintSolving._
/**
 * Created by lorilan on 25/09/14.
 */
abstract class ConstraintSolvingSearchEngineDecisionMaker[Kind <: NodeKind[Kind]]
(solverBuilder : SolverBuilder[Kind],
  solverLogger : Logger[Int])
  extends SearchEngine[NodeChoice[Kind]]
  with DecisionMaker[Kind]{

  val initialState = new CSInitialSearchState(this, solverBuilder(graph, solverLogger, this))

  val logger : Logger[Int]
  val violationsKindPriority : List[Kind]

  def violationTarget(k: Option[NodeType] => Unit) {
    def aux : List[Kind] => Iterator[NodeType] =  {
      case topPriority :: tl =>
        val it = graph.filter{ n =>
          n.kind == topPriority && (n.wrongUsers.nonEmpty ||
            n.isWronglyContained)
        }
        if(it.hasNext) it
        else aux(tl)

      case List() => graph.filter{ n => n.wrongUsers.nonEmpty ||
        n.isWronglyContained }
    }

    val violationTargets = new ConstraintSolvingNodesChoice(graph.transformations.recording, k,
      mutable.Set[NodeType]() ++ aux(violationsKindPriority),
      mutable.Set[NodeType]())

    newCurrentState(violationTargets)

    if(violationTargets.remainingChoices.nonEmpty)
      newCurrentState(violationTargets)
    else {
      val fs = currentState.createNextState(violationTargets)
      finalStates += fs

      /*val fs = currentState.createNextState(k, violationTargets)
      finalStates.find( s =>
          s.internal.recording.produceSameGraph(fs.internal.recording)) match {
        case Some(s) =>
          logger.writeln("final states %s and %s have same graph".format(s.uuid(), currentState.uuid()))
        case None =>
          logger.writeln("final state found : " + fs.uuid() )
          finalStates += fs
          fs.executeNextChoice() // this will call step one last time
      }*/
    }
  }

  def abstractionKindAndPolicy(impl : NodeType)(k : Option[(Kind, AbstractionPolicy)] => Unit) = {
    import impl.kind.{abstractionPolicies, abstractKinds}

      if(abstractionPolicies.nonEmpty)
          k (Some((abstractKinds(abstractionPolicies.head).head, abstractionPolicies.head)))
      else k(None)
    /*val choices = abstractionPolicies.map{ p => abstractKinds(p).map{kind => (kind,p) } }.flatten

    newCurrentState(new ConstraintSolvingAbstractionChoice(graph.transformations.recording, k,
    mutable.Set[(Kind, AbstractionPolicy)]() ++ choices,
      mutable.Set[(Kind, AbstractionPolicy)]()))*/
  }

  def chooseNode(context : => String,
                 predicate : NodeType => Boolean,
                 k : Option[NodeType] => Unit) {
    //println(context)
    newCurrentState(new ConstraintSolvingNodesChoice(graph.transformations.recording, k,
      mutable.Set[NodeType]() ++ graph.filter(predicate),
      mutable.Set[NodeType]()))
  }

  def modifyConstraints(sources : NodeSet[Kind], target : NodeType){}

}
