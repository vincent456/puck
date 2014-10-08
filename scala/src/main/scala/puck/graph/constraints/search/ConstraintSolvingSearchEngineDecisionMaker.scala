package puck.graph.constraints.search

import puck.graph.backTrack.Recording
import puck.graph.constraints.{AbstractionPolicy, DecisionMaker, NodeSet}
import puck.graph.NodeKind
import puck.search.{SearchState, SearchEngine}
import puck.util.Logger

import scala.collection.mutable

object ConstraintSolving {
  type NodeChoice[Kind <: NodeKind[Kind]] = ConstraintSolvingNodesChoice[Kind]
  type AbsChoice[Kind <: NodeKind[Kind]] = ConstraintSolvingAbstractionChoice[Kind]
  type FinalState[Kind <: NodeKind[Kind]] = SearchState[Recording[Kind], _]
}

/**
 * Created by lorilan on 25/09/14.
 */
abstract class ConstraintSolvingSearchEngineDecisionMaker[Kind <: NodeKind[Kind]]
(solverBuilder : SolverBuilder[Kind],
 solverLogger : Logger[Int])
  extends SearchEngine[Recording[Kind]]
  with DecisionMaker[Kind]{

  val initialState = new CSInitialSearchState(this, solverBuilder(graph, solverLogger, this))

  val logger : Logger[Int]
  val violationsKindPriority : List[Kind]

  def violationTarget(k: Option[NodeType] => Unit) {
    def findTargets : List[Kind] => Iterator[NodeType] =  {
      case topPriority :: tl =>
        val it = graph.filter{ n =>
          n.kind == topPriority && (n.wrongUsers.nonEmpty ||
            n.isWronglyContained)
        }
        if(it.hasNext) it
        else findTargets(tl)

      case List() => graph.filter{ n => n.wrongUsers.nonEmpty ||
        n.isWronglyContained }
    }

    val targets = findTargets(violationsKindPriority).toList

    lazy val targetsChoice =  new ConstraintSolvingNodesChoice(k,
      mutable.Set[NodeType]() ++ targets,
      mutable.Set[NodeType]())

    targets match {
      case List() =>
        k(None) // will call doMerge
        newCurrentState(graph.transformations.recording, targetsChoice)
        finalStates += currentState
      case List(oneTarget) =>
        k(Some(oneTarget))
      case _ => //more than one target
        newCurrentState(graph.transformations.recording, targetsChoice)

    }
  }

  def abstractionKindAndPolicy(impl : NodeType)(k : Option[(Kind, AbstractionPolicy)] => Unit) = {
    import impl.kind.{abstractionPolicies, abstractKinds}

    if(abstractionPolicies.isEmpty) {
      k(None)
    }
    else{
        k(Some((abstractKinds(abstractionPolicies.head).head,
          abstractionPolicies.head)))
    }

    /*var needSearch = true

    if(abstractionPolicies.isEmpty) {
      needSearch = false
      k(None)
    }
    else if(abstractionPolicies.tail.isEmpty){
      val absk = abstractKinds(abstractionPolicies.head)
      if(absk.tail.isEmpty) {
        needSearch = false
        k(Some((absk.head, abstractionPolicies.head)))
      }
    }

    if(needSearch) {
      val choices = abstractionPolicies.map { p => abstractKinds(p).map { kind => (kind, p)}}.flatten

      newCurrentState(graph.transformations.recording,
        new ConstraintSolvingAbstractionChoice(k,
          mutable.Set[(Kind, AbstractionPolicy)]() ++ choices,
          mutable.Set[(Kind, AbstractionPolicy)]()))
    }*/
  }

  def chooseNode(context : => String,
                 predicate : NodeType => Boolean,
                 k : Option[NodeType] => Unit) {
    //println(context)
    newCurrentState(graph.transformations.recording,
      new ConstraintSolvingNodesChoice(k,
      mutable.Set[NodeType]() ++ graph.filter(predicate),
      mutable.Set[NodeType]()))
  }

  def modifyConstraints(sources : NodeSet[Kind], target : NodeType){}

}
