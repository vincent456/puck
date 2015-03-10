/*
package puck.gui

import puck.graph.NodeKind
import puck.graph.constraints.{DecisionMaker, AbstractionPolicy}
import puck.graph.io.FilesHandler

/**
 * Created by lorilan on 04/06/14.
 */
class GUIDecisionMaker(val filesHandler : FilesHandler)
  extends DecisionMaker{

  val graph = filesHandler.graph

  override def toString = "User Decision Maker"


  def violationTarget(graph : GraphT)
                     (k: Option[NIdT] => Unit) : Unit = ???

  def abstractionKindAndPolicy(graph : GraphT, impl : NIdT)
                              (k : Option[(NodeKind, AbstractionPolicy)] => Unit) : Unit = ???

  def chooseNode(graph : GraphT, predicate : PredicateT)
                (k : Option[NIdT] => Unit) : Unit = ???


  //val fallback = filesHandler.decisionMaker()

/*  def violationTarget(k: Option[NodeType] => Unit) {
    fallback.violationTarget(k)
  }


  def chooseNode(predicate : NodeType => Boolean)
                (k : Option[NodeType] => Unit) : Unit =
    k(NodeChooser(LiteralNodeSet(graph.filter(predicate)), "Empty context in GUIDecisionMaker"))

  def modifyConstraints(sources : NodeSet[Kind], target : NodeType) {
    ConstraintExceptionFrame(sources, target)
  }

  def abstractionKindAndPolicy(impl : NodeType) =
    AbstractionKindAndPolicyChooser(impl)*/
}
*/
