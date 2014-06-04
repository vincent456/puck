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

trait DecisionMaker{

  def abstractionKindAndPolicy(impl : AGNode) : (NodeKind, AbstractionPolicy)

  def chooseNode(graph : AccessGraph)(predicate : AGNode => Boolean) : Option[AGNode]

  def grantContainingAuth(container : AGNode, content : AGNode,
                           violatedScopeConstraints : List[ScopeConstraint],
                           violatedElementConstraints : List[ElementConstraint]) : Boolean

  def grantUsesAuth(user : AGNode, usee : AGNode,
                    violatedScopeConstraints : List[ScopeConstraint],
                    violatedElementConstraints : List[ElementConstraint]) : Boolean

}
