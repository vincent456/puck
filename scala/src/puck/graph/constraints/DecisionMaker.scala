package puck.graph.constraints

import puck.graph.{AccessGraph, NodeKind, AGNode}
import scala.concurrent.Future


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

  /**
   * to return true, the method must first modify the constraint acccordingly
   */
  def grantContainingAuth(container : AGNode, content : AGNode) : Boolean
  def grantUsesAuth(user : AGNode, usee : AGNode) : Boolean

}
