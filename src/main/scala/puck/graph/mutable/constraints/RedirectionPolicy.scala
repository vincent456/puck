package puck.graph.mutable.constraints

/**
 * Created by lorilan on 04/06/14.
 */
sealed abstract class RedirectionPolicy
sealed abstract class AbstractionPolicy extends RedirectionPolicy

case class DelegationAbstraction() extends AbstractionPolicy {
  override val toString = "DelegationAbstraction"
}
case class SupertypeAbstraction() extends AbstractionPolicy{
  override val toString = "SuperTypeAbstraction"
}
case class Move() extends RedirectionPolicy