package puck.graph.constraints


sealed abstract class RedirectionPolicy
sealed abstract class AbstractionPolicy extends RedirectionPolicy

case object DelegationAbstraction extends AbstractionPolicy

case object SupertypeAbstraction extends AbstractionPolicy //with AbstractionKind

case object NotAnAbstraction extends RedirectionPolicy