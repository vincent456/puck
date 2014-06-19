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

  val graph : AccessGraph

  def containViolationTarget : Option[AGNode]

  def usesViolationTarget : Option[AGNode]

  def abstractionKindAndPolicy(impl : AGNode) : (NodeKind, AbstractionPolicy)

  def chooseNode(context : String)(predicate : AGNode => Boolean) : Option[AGNode]

  def modifyConstraints(sources : NodeSet, target : AGNode) : Unit

}
