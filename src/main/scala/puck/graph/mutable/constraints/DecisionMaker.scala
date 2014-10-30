package puck.graph.mutable.constraints

import puck.graph.constraints.AbstractionPolicy
import puck.graph.mutable.{AGNode, AccessGraph, NodeKind}


/**
 * Created by lorilan on 28/05/14.
 */

sealed class AuthorisationPolicy
case class FacadeAuth() extends AuthorisationPolicy
case class LocalFriendAuth() extends AuthorisationPolicy
case class GlobalFriendAuth() extends AuthorisationPolicy
case class NoAuthorisation() extends AuthorisationPolicy

trait DecisionMaker[Kind <: NodeKind[Kind]]{
  type NodeType = AGNode[Kind]

  val graph : AccessGraph[Kind]

  def violationTarget(k: Option[NodeType] => Unit) : Unit

  def abstractionKindAndPolicy(impl : NodeType)(k : Option[(Kind, AbstractionPolicy)] => Unit) : Unit

  def chooseNode(predicate : NodeType => Boolean)
                (k : Option[NodeType] => Unit) : Unit

  def modifyConstraints(sources : NodeSet[Kind], target : NodeType) : Unit

}
