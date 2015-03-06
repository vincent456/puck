package puck.graph.constraints.search

import puck.graph.{ConcreteNode, ResultT, NodeKind}
import puck.graph.constraints.{AbstractionPolicy, DecisionMaker}
import puck.search.SearchEngine
import puck.util.PuckLogger

object ConstraintSolving {
  type NodeChoice = ConstraintSolvingNodesChoice
  type AbsChoice = ConstraintSolvingAbstractionChoice
}

/**
 * Created by lorilan on 25/09/14.
 */
abstract class ConstraintSolvingSearchEngineDecisionMaker
  extends SearchEngine[ResultT]
  with DecisionMaker {

  def logger : PuckLogger

  //def initialState = new CSInitialSearchState(this, solverBuilder(graph, this))

  val violationsKindPriority : Seq[NodeKind]

  /*def violationTarget(graph : GraphT)
                     (k: Option[NIdT] => Unit) : Unit = {

    implicit val g = graph

    def findTargets(l : Seq[NodeKind]) : Iterator[DGNode] =  l match {
      case topPriority :: tl =>
        val it = graph.nodes.iterator filter { n =>
          n.kind == topPriority && (g.wrongUsers(n.id).nonEmpty ||
            g.isWronglyContained(n.id))
        }
        if(it.hasNext) it
        else findTargets(tl)

      case Seq() => graph.nodes.iterator filter { n => g.wrongUsers(n.id).nonEmpty ||
        g.isWronglyContained(n.id) }
    }

    val targets = (findTargets(violationsKindPriority) map (_.id)).toSeq

    lazy val targetsChoice = ConstraintSolvingNodesChoice(k, targets)

    targets match {
      case Seq() => k(None) // will call doMerge

      case Seq(oneTarget) =>
        k(Some(oneTarget))
      case _ => //more than one target
        newCurrentState((graph, graph.recording), targetsChoice)

    }
  }*/

  def violationTarget(graph : GraphT)
                     (k: Option[ConcreteNode] => Unit) : Unit = {


    def findTargets(l : Seq[NodeKind]) : Option[ConcreteNode] =  l match {
      case topPriority :: tl =>
        graph.concreteNodes.iterator find { n =>
          n.kind == topPriority && (graph.wrongUsers(n.id).nonEmpty ||
            graph.isWronglyContained(n.id))
        } match {
          case None => findTargets(tl)
          case st @ Some(_) => st
        }

      case Seq() => graph.concreteNodes find { n => graph.wrongUsers(n.id).nonEmpty ||
        graph.isWronglyContained(n.id) }
    }

    k(findTargets(violationsKindPriority))
  }

  def abstractionKindAndPolicy(graph : GraphT, impl : ConcreteNode)
                              (k : Option[(NodeKind, AbstractionPolicy)] => Unit) : Unit = {

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
      val choices = abstractionPolicies.map { p => abstractKinds(p).map { kind => Some((kind, p)) }}.flatten

      newCurrentState((graph, graph.recording),
        new ConstraintSolvingAbstractionChoice(k,
          Set[Option[(NodeKind, AbstractionPolicy)]]() ++ choices,
          Set[Option[(NodeKind, AbstractionPolicy)]]()))
    }
    else k(karg)*/

  }

  def chooseNode(graph : GraphT, predicate : PredicateT)
                (k : Option[NIdT] => Unit) : Unit = {
    val choices = graph.nodesId.filter(predicate(graph,_)).toSeq
    if(choices.nonEmpty)
      newCurrentState((graph, graph.recording),
        ConstraintSolvingNodesChoice.includeNoneChoice(k, choices))
    else
      k(None)
  }

/*  def modifyConstraints(sources : NodeSet[Kind], target : NodeType){}*/

}
