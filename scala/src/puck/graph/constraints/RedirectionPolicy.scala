package puck.graph.constraints

/**
 * Created by lorilan on 04/06/14.
 */
sealed abstract class RedirectionPolicy
sealed abstract class AbstractionPolicy extends RedirectionPolicy

case class DelegationAbstraction() extends AbstractionPolicy
case class SupertypeAbstraction() extends AbstractionPolicy
case class Move() extends RedirectionPolicy