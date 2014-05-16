package puck.graph

/**
 * Created by lorilan on 12/05/14.
 */
package object constraints {
  sealed abstract class Constraint
  case class HideScope (scope : AGNode,
                        facades : List[AGNode],
                        interlopers :List[AGNode],
                        friends : List[AGNode]) extends Constraint

  case class AreFriendsOf(friends : List[AGNode], befriended : AGNode)
    extends Constraint

  case class HideElement(elt : AGNode,
                         interlopers : List[AGNode],
                         friends : List[AGNode]) extends Constraint



}
/*
 classes used to store the constraint in the node
 */
class ScopeConstraint(val facades: List[AGNode],
                      val interlopers: List[AGNode],
                      val friends : List[AGNode]){

  override def toString =
  "hideScope("+
  facades.mkString("[", ",\n", "],\n") +
  interlopers.mkString("[", ",\n", "],\n") +
  friends.mkString("[", ",\n", "]).")


}
class ElementConstraint(val interlopers :List[AGNode],
                         val friends : List[AGNode]){

  override def toString =
  "hideElement("+
    interlopers.mkString("[", ",\n", "],\n") +
    friends.mkString("[", ",\n", "]).")
}