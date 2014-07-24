package puck.graph.constraints

import puck.graph.{AGNode, AccessGraph, NodeKind}


/**
 * Created by lorilan on 03/06/14.
 */
abstract class DefaultDecisionMaker[Kind <: NodeKind[Kind]](val graph : AccessGraph[Kind]) extends DecisionMaker[Kind]{


  override def toString = "Default Strategy"

  val violationsKindPriority : List[Kind]

  def violationTarget(k: Option[NodeType] => Unit){
    def aux (priorities : List[Kind]) : Option[NodeType] = priorities match {
      case topPriority :: tl => graph.iterator.find{ (n : NodeType) =>
        n.kind == topPriority && (n.wrongUsers.nonEmpty ||
          n.isWronglyContained)
      } match {
        case None => aux(tl)
        case res => res
      }
      case List() => graph.iterator.find{ n => n.wrongUsers.nonEmpty ||
        n.isWronglyContained }
    }

    k(aux(violationsKindPriority))
  }


  def abstractionKindAndPolicy(impl : NodeType) = {
    val policy = impl.kind.abstractionPolicies.head
    (impl.kind.abstractKinds(policy).head, policy)
  }

  def chooseNode(context : => String,
                 predicate : NodeType => Boolean,
                 k : Option[NodeType] => Unit) : Unit = {
    println(context)
    val found = graph.iterator.find(predicate)
    println("found : " + found)
    k(found)
  }

  def modifyConstraints(sources : NodeSet[Kind], target : NodeType){}

}
