package puck.javaAG

import puck.graph.{AccessGraph, AGNode}
import puck.graph.constraints.{LiteralNodeSet, DecisionMaker}


/**
 * Created by lorilan on 03/06/14.
 */
object DefaultDecisionMaker extends DecisionMaker{

  override def toString = "Default Strategy"

  def abstractionKindAndPolicy(impl : AGNode) = {
    val policy = impl.kind.abstractionPolicies.head
    (impl.kind.abstractKinds(policy).head, policy)
  }

  def chooseNode(graph : AccessGraph, context : String)(predicate : AGNode => Boolean) : Option[AGNode] = {
    println(context)
    val found = graph.iterator.find(predicate)
    println("found : " + found)
    found
  }

  def grantContainingAuth(container : AGNode, content : AGNode) : Boolean ={
    false
  }

  def grantUsesAuth(user : AGNode, usee : AGNode) =
    throw new Error("grantUsesAuth not implemented")
}
