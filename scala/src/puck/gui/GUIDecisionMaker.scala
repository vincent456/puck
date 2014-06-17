package puck.gui

import puck.graph.constraints._
import puck.graph.{AccessGraph, AGNode}
import puck.javaAG.DefaultDecisionMaker
import puck.gui.decisionsFrames.{NodeChooser, ConstraintExceptionFrame, AbstractionKindAndPolicyChooser}
import scala.concurrent.{Await}
import scala.concurrent.duration.Duration

/**
 * Created by lorilan on 04/06/14.
 */
object GUIDecisionMaker extends DecisionMaker{



  override def toString = "User Decision Maker"

  def abstractionKindAndPolicy(impl : AGNode) = {

    AbstractionKindAndPolicyChooser(impl)
    /*val policy = impl.kind.abstractionPolicies.head
    (impl.kind.abstractKinds(policy).head, policy)*/
    //Await.result(frame.result, Duration.Inf)
  }

  def chooseNode(graph : AccessGraph, context : String)(predicate : AGNode => Boolean) : Option[AGNode] =
    NodeChooser(LiteralNodeSet(graph.filter(predicate)), context)

  def grantContainingAuth(container : AGNode, content : AGNode) : Boolean = {
    ConstraintExceptionFrame(LiteralNodeSet(container), content)
    //Await.result(frame.result, Duration.Inf)
  }

  def grantUsesAuth(user : AGNode, usee : AGNode) =
    DefaultDecisionMaker.grantUsesAuth(user, usee)

}
