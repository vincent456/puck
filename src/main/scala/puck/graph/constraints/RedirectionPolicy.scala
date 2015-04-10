package puck.graph.constraints

sealed abstract class RedirectionPolicy
sealed abstract class AbstractionPolicy extends RedirectionPolicy

case object DelegationAbstraction extends AbstractionPolicy /*{
  //override val toString = "DelegationAbstraction"
}*/
case object SupertypeAbstraction extends AbstractionPolicy/*{
 // override val toString = "SuperTypeAbstraction"
}*/
case object NotAnAbstraction extends RedirectionPolicy