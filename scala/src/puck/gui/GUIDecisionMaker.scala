package puck.gui

import puck.graph.constraints._
import puck.graph.{NodeKind, AccessGraph, AGNode}
import puck.javaAG.{JavaDefaultDecisionMaker, JavaNodeKind, JavaAccessGraph}
import puck.gui.decisionsFrames.{NodeChooser, ConstraintExceptionFrame, AbstractionKindAndPolicyChooser}
import scala.concurrent.{Await}
import scala.concurrent.duration.Duration

/**
 * Created by lorilan on 04/06/14.
 */
class GUIDecisionMaker(val graph : JavaAccessGraph) extends DecisionMaker[JavaNodeKind]{

  val fallback = new JavaDefaultDecisionMaker(graph)

  override def toString = "User Decision Maker"

  def violationTarget(k: Option[NodeType] => Unit) {
    fallback.violationTarget(k)
  }

  def abstractionKindAndPolicy(impl : NodeType) =
    AbstractionKindAndPolicyChooser(impl)

  def chooseNode(context : => String,
                 predicate : NodeType => Boolean,
                 k : Option[NodeType] => Unit) : Unit =
    k(NodeChooser(LiteralNodeSet(graph.filter(predicate)), context))

  def modifyConstraints(sources : NodeSet[JavaNodeKind], target : NodeType) {
    ConstraintExceptionFrame(sources, target)
  }

}
