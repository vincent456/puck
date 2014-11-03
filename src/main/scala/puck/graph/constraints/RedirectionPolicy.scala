package puck.graph.constraints

/**
 * Created by lorilan on 04/06/14.
 */
sealed abstract class RedirectionPolicy
sealed abstract class AbstractionPolicy extends RedirectionPolicy

case object DelegationAbstraction extends AbstractionPolicy /*{
  //override val toString = "DelegationAbstraction"
}*/
case object SupertypeAbstraction extends AbstractionPolicy/*{
 // override val toString = "SuperTypeAbstraction"
}*/
case object Move extends RedirectionPolicy