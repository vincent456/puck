package puck.javaAG

import puck.graph.{NodeKind, AccessGraph, AGNode}
import puck.graph.constraints.{NodeSet, LiteralNodeSet, DecisionMaker}
import puck.javaAG.JavaNodeKind.{Interface, Class, Constructor, Field}


/**
 * Created by lorilan on 03/06/14.
 */
class DefaultDecisionMaker(val graph : AccessGraph) extends DecisionMaker{


  override def toString = "Default Strategy"

  val violationsKindPriority = List[NodeKind](Field(), Constructor(),
    Class(), Interface())

  def violationTarget(k: Option[AGNode] => Unit){
    def aux (priorities : List[NodeKind]) : Option[AGNode] = priorities match {
      case topPriority :: tl => graph.iterator.find{ (n : AGNode) =>
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


  def abstractionKindAndPolicy(impl : AGNode) = {
    val policy = impl.kind.abstractionPolicies.head
    (impl.kind.abstractKinds(policy).head, policy)
  }

  def chooseNode(context : => String,
                 predicate : AGNode => Boolean,
                 k : Option[AGNode] => Unit) : Unit = {
    println(context)
    val found = graph.iterator.find(predicate)
    println("found : " + found)
    k(found)
  }

  def modifyConstraints(sources : NodeSet, target : AGNode){}

}
