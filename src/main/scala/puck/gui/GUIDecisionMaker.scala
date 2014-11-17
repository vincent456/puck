package puck.gui

import puck.graph.constraints.AbstractionPolicy
import puck.graph.mutable.NodeKind
import puck.graph.mutable.constraints._
import puck.graph.mutable.io.FilesHandler
import puck.gui.search.decisionsFrames.{NodeChooser, ConstraintExceptionFrame, AbstractionKindAndPolicyChooser}

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


  def chooseNode(predicate : NodeType => Boolean)
                (k : Option[NodeType] => Unit) : Unit =
    k(NodeChooser(LiteralNodeSet(graph.filter(predicate)), "Empty context in GUIDecisionMaker"))

  def modifyConstraints(sources : NodeSet[Kind], target : NodeType) {
    ConstraintExceptionFrame(sources, target)
  }

/*
  def abstractionKindAndPolicy(impl : NodeType) =
    AbstractionKindAndPolicyChooser(impl)
*/


  def abstractionKindAndPolicy(impl: NodeType)(k: (Option[(Kind, AbstractionPolicy)]) => Unit): Unit = ???
}
