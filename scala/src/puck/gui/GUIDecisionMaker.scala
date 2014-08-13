package puck.gui

import puck.graph.NodeKind
import puck.graph.constraints._
import puck.graph.io.FilesHandler
import puck.gui.decisionsFrames.{NodeChooser, ConstraintExceptionFrame, AbstractionKindAndPolicyChooser}

/**
 * Created by lorilan on 04/06/14.
 */
class GUIDecisionMaker[Kind <: NodeKind[Kind]](val filesHandler : FilesHandler[Kind])
  extends DecisionMaker[Kind]{

  val graph = filesHandler.graph
  val fallback = filesHandler.decisionMaker()

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

  def modifyConstraints(sources : NodeSet[Kind], target : NodeType) {
    ConstraintExceptionFrame(sources, target)
  }

}
