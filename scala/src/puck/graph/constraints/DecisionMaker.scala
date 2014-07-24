package puck.graph.constraints

import puck.graph.{AccessGraph, NodeKind, AGNode}


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

  def abstractionKindAndPolicy(impl : NodeType) : (Kind, AbstractionPolicy)

  def chooseNode(context : => String,
                 predicate : NodeType => Boolean,
                 k : Option[NodeType] => Unit) : Unit

  def modifyConstraints(sources : NodeSet[Kind], target : NodeType) : Unit

}
