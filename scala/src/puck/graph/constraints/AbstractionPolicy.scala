package puck.graph.constraints

/**
 * Created by lorilan on 04/06/14.
 */
sealed abstract class AbstractionPolicy

case class Delegation() extends AbstractionPolicy
case class Supertype() extends AbstractionPolicy
