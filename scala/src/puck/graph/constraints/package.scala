package puck.graph

/**
 * Created by lorilan on 12/05/14.
 */
package object constraints {
  abstract class Constraint
  case class HideScope (scope : AGNode,
                        facades : List[AGNode],
                        interlopers :List[AGNode],
                        friends : List[AGNode]) extends Constraint

  case class IsFriendOf(friend : List[AGNode], befriended : List[AGNode])
    extends Constraint


}
