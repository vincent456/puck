package puck.graph.mutable.constraints

import puck.graph.mutable.{AccessGraph, NodeKind}


/**
 * Created by lorilan on 03/06/14.
 */
abstract class DefaultDecisionMaker[Kind <: NodeKind[Kind]](val graph : AccessGraph[Kind]) extends DecisionMaker[Kind]{

  override def toString = "Default Strategy"

  val violationsKindPriority : List[Kind]

  def violationTarget(k: Option[NodeType] => Unit){
    def aux (priorities : List[Kind]) : Option[NodeType] = priorities match {
      case topPriority :: tl => graph.find{ (n : NodeType) =>
        n.kind == topPriority && (n.wrongUsers.nonEmpty ||
          n.isWronglyContained)
      } match {
        case None => aux(tl)
        case res => res
      }
      case List() => graph.find{ n => n.wrongUsers.nonEmpty ||
        n.isWronglyContained }
    }

    k(aux(violationsKindPriority))
}


  def abstractionKindAndPolicy(impl : NodeType) (k : Option[(Kind, AbstractionPolicy)] => Unit)= {

    if(impl.kind.abstractionPolicies.isEmpty)
      k(None)
    else {
      val policy = impl.kind.abstractionPolicies.head
      k(Some(impl.kind.abstractKinds(policy).head, policy))
    }
  }

  def chooseNode(predicate : NodeType => Boolean)
                (k : Option[NodeType] => Unit) : Unit = {
    val found = graph.iterator.find(predicate)
    graph.logger.writeln("found : " + found)
    k(found)
  }

  def modifyConstraints(sources : NodeSet[Kind], target : NodeType){}

}
