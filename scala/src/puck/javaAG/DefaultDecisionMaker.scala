package puck.javaAG

import puck.graph.{AccessGraph, AGNode}
import puck.graph.constraints.{ElementConstraint, ScopeConstraint, DecisionMaker}
import scala.concurrent.Future

/**
 * Created by lorilan on 03/06/14.
 */
object DefaultDecisionMaker extends DecisionMaker{

  override def toString = "Default Strategy"

  def abstractionKindAndPolicy(impl : AGNode) = {
    val policy = impl.kind.abstractionPolicies.head
    (impl.kind.abstractKinds(policy).head, policy)
  }

  def chooseNode(graph : AccessGraph)(predicate : AGNode => Boolean) : Option[AGNode] = {
      graph.iterator.find(predicate)
  }

  def grantContainingAuth(container : AGNode, content : AGNode,
                          violatedScopeConstraints : List[ScopeConstraint],
                          violatedElementConstraints : List[ElementConstraint]) : Boolean ={
    false
  }

  def grantUsesAuth(user : AGNode, usee : AGNode,
                    violatedScopeConstraints : List[ScopeConstraint],
                    violatedElementConstraints : List[ElementConstraint]) =
    throw new Error("grantUsesAuth not implemented")
}
