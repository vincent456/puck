package puck.graph.constraints.search

import puck.graph.{ResultT, AGNode, NodeKind}
import puck.graph.constraints.{AbstractionPolicy, DecisionMaker}
import puck.search.{Evaluator, SearchState, SearchEngine}
import puck.util.PuckLogger

import scala.collection.mutable

object ConstraintSolving {
  type NodeChoice[Kind <: NodeKind[Kind], T] = ConstraintSolvingNodesChoice[Kind, T]
  type AbsChoice[Kind <: NodeKind[Kind], T] = ConstraintSolvingAbstractionChoice[Kind, T]
}

/**
 * Created by lorilan on 25/09/14.
 */
abstract class ConstraintSolvingSearchEngineDecisionMaker[Kind <: NodeKind[Kind], T]
  extends SearchEngine[ResultT[Kind, T]]
  with DecisionMaker[Kind, T] with Evaluator[ResultT[Kind, T]]{

  def logger : PuckLogger

  //def initialState = new CSInitialSearchState(this, solverBuilder(graph, this))

  val violationsKindPriority : Seq[Kind]

  def evaluate(s : SearchState[ResultT[Kind, T]]): Double ={
    if(currentState!=s)
      s.setAsCurrentState()
    val graph = s.result._1
    graph.coupling
  }

  def violationTarget(graph : GraphT)
                     (k: Option[NIdT] => Unit) : Unit = {

    def findTargets(l : Seq[Kind]) : Iterator[AGNode[Kind, T]] =  l match {
      case topPriority :: tl =>
        val it = graph.nodes.iterator filter { n =>
          n.kind == topPriority && (n.wrongUsers.nonEmpty ||
            n.isWronglyContained)
        }
        if(it.hasNext) it
        else findTargets(tl)

      case Seq() => graph.nodes.iterator filter { n => n.wrongUsers.nonEmpty ||
        n.isWronglyContained }
    }

    val targets = (findTargets(violationsKindPriority) map (_.id)).toSeq

    lazy val targetsChoice =  new ConstraintSolvingNodesChoice[Kind, T](k,
      mutable.Set[NIdT]() ++ targets,
      mutable.Set[NIdT]())

    targets match {
      case Seq() => k(None) // will call doMerge

      case Seq(oneTarget) =>
        k(Some(oneTarget))
      case _ => //more than one target
        newCurrentState((graph, graph.recording), targetsChoice)

    }
  }

  def abstractionKindAndPolicy(graph : GraphT, implId : NIdT)
                              (k : Option[(Kind, AbstractionPolicy)] => Unit) : Unit = {
    val impl = graph.getNode(implId)
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

  def chooseNode(graph : GraphT, predicate : PredicateT)
                (k : Option[NIdT] => Unit) : Unit =
    newCurrentState((graph, graph.recording),
      new ConstraintSolvingNodesChoice[Kind, T](k,
      mutable.Set[NIdT]() ++ graph.nodesId.filter(predicate(graph,_)).toSeq,
      mutable.Set[NIdT]()))

/*  def modifyConstraints(sources : NodeSet[Kind], target : NodeType){}*/

}
