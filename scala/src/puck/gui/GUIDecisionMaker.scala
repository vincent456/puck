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
class GUIDecisionMaker(val graph : AccessGraph) extends DecisionMaker{

  val fallback = new DefaultDecisionMaker(graph)

  override def toString = "User Decision Maker"

  def violationTarget(k: Option[AGNode] => Unit) {
    fallback.violationTarget(k)
  }

  def abstractionKindAndPolicy(impl : AGNode) =
    AbstractionKindAndPolicyChooser(impl)

  def chooseNode(context : => String,
                 predicate : AGNode => Boolean,
                 k : Option[AGNode] => Unit) : Unit =
    k(NodeChooser(LiteralNodeSet(graph.filter(predicate)), context))

  def modifyConstraints(sources : NodeSet, target : AGNode) {
    ConstraintExceptionFrame(sources, target)
  }

}
