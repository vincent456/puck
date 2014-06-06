package puck.gui

import puck.graph.constraints.{RedirectionPolicy, ElementConstraint, ScopeConstraint, DecisionMaker}
import puck.graph.{NodeKind, AccessGraph, AGNode}
import puck.javaAG.DefaultDecisionMaker
import puck.gui.decisionsFrames.{ContainingAuthChooser, AbstractionKindAndPolicyChooser}
import scala.concurrent.{Promise, Await}
import scala.concurrent.duration.Duration

/**
 * Created by lorilan on 04/06/14.
 */
object GUIDecisionMaker extends DecisionMaker{

  override def toString = "User Decision Maker"

  def abstractionKindAndPolicy(impl : AGNode) = {

    val frame = new AbstractionKindAndPolicyChooser(impl)
    frame.visible = true
    /*val policy = impl.kind.abstractionPolicies.head
    (impl.kind.abstractKinds(policy).head, policy)*/
    Await.result(frame.result, Duration.Inf)
  }

  def chooseNode(graph : AccessGraph)(predicate : AGNode => Boolean) : Option[AGNode] =
    DefaultDecisionMaker.chooseNode(graph)(predicate)

  def grantContainingAuth(container : AGNode, content : AGNode,
                          violatedScopeConstraints : List[ScopeConstraint],
                          violatedElementConstraints : List[ElementConstraint]) : Boolean = {

    val frame = new ContainingAuthChooser(container, content,
                            violatedScopeConstraints,
                            violatedElementConstraints)
    frame.visible = true

    Await.result(frame.result, Duration.Inf)

  }


  def grantUsesAuth(user : AGNode, usee : AGNode,
                    violatedScopeConstraints : List[ScopeConstraint],
                    violatedElementConstraints : List[ElementConstraint]) =
    DefaultDecisionMaker.grantUsesAuth(user, usee,
      violatedScopeConstraints,
      violatedElementConstraints)

}
