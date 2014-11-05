package puck.graph.mutable.constraints.search

import puck.graph.AGError
import puck.graph.constraints.AbstractionPolicy
import puck.graph.mutable.backTrack.Recording
import puck.graph.mutable.NodeKind
import puck.graph.mutable.constraints.{DecisionMaker, NodeSet}
import puck.search.{Evaluator, SearchState, SearchEngine}
import puck.util.PuckLogger

import scala.collection.mutable

object ConstraintSolving {
  type NodeChoice[Kind <: NodeKind[Kind]] = ConstraintSolvingNodesChoice[Kind]
  type AbsChoice[Kind <: NodeKind[Kind]] = ConstraintSolvingAbstractionChoice[Kind]
  type FinalState[Kind <: NodeKind[Kind]] = SearchState[Recording[Kind]]
}

/**
 * Created by lorilan on 25/09/14.
 */
abstract class ConstraintSolvingSearchEngineDecisionMaker[Kind <: NodeKind[Kind]]
(solverBuilder : SolverBuilder[Kind])
  extends SearchEngine[Recording[Kind]]
  with DecisionMaker[Kind] with Evaluator[Recording[Kind]]{

  val logger : PuckLogger = graph.logger


  //val initialState = new CSInitialSearchState(this, solverBuilder(graph, this))

  val violationsKindPriority : List[Kind]

  def evaluate(s : SearchState[Recording[Kind]]): Double ={
    if(currentState!=s)
      s.setAsCurrentState()

    graph.coupling
  }

  def violationTarget(k: Option[NodeType] => Unit) {

    def findTargets(l : List[Kind]) : Iterator[NodeType] =  l match {
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
          throw new AGError("currentState should be added to final states")
        //finalStates += currentState
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

    /*val (needSearch, karg) =
      if(abstractionPolicies.isEmpty) {
        (false, None)
    }
    else if(abstractionPolicies.tail.isEmpty){
      val absk = abstractKinds(abstractionPolicies.head)
      if(absk.tail.isEmpty) {
        (false, Some((absk.head, abstractionPolicies.head)))
      }
      else (true, None)

    }
    else (true, None)


    if(needSearch) {
      val choices = abstractionPolicies.map { p => abstractKinds(p).map { kind => (kind, p)}}.flatten

      newCurrentState(graph.transformations.recording,
        new ConstraintSolvingAbstractionChoice(k,
          mutable.Set[(Kind, AbstractionPolicy)]() ++ choices,
          mutable.Set[(Kind, AbstractionPolicy)]()))
    }
    else k(karg)*/

  }

  def chooseNode(predicate : NodeType => Boolean)
                (k : Option[NodeType] => Unit) {
    //println(context)
    newCurrentState(graph.transformations.recording,
      new ConstraintSolvingNodesChoice(k,
      mutable.Set[NodeType]() ++ graph.filter(predicate).toSeq,
      mutable.Set[NodeType]()))
  }

  def modifyConstraints(sources : NodeSet[Kind], target : NodeType){}

}
