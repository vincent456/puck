package puck.gui

import puck.graph.constraints._
import puck.javaAG.nodeKind.JavaNodeKind
import puck.javaAG.{JavaDefaultDecisionMaker, JavaAccessGraph}
import puck.gui.decisionsFrames.{NodeChooser, ConstraintExceptionFrame, AbstractionKindAndPolicyChooser}
import puck.util.NoopLogger

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
