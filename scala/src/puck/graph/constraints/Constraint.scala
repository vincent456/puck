package puck.graph.constraints

import puck.graph.AGNode

/**
 * Created by lorilan on 03/06/14.
 */
abstract class Constraint(val owner : AGNode,
                          val interlopers :List[AGNode],
                          private var friends0 : List[AGNode]){

  def friends : List[AGNode]  = friends0
  def friends_+=(friend : AGNode) = friends0 = friend :: friends0

}

class ScopeConstraint(scope : AGNode,
                      private var facades0: List[AGNode],
                      iss: List[AGNode],
                      frs : List[AGNode]) extends Constraint(scope, iss, frs){

  def facades : List[AGNode] = facades0
  def facades_+=(facade : AGNode) = facades0 = facade :: facades0

  override def toString =
    "hideScope("+ scope + ",\n" +
      facades.mkString("[", ",\n", "],\n") +
      interlopers.mkString("[", ",\n", "],\n") +
      friends.mkString("[", ",\n", "]).")

}

class ElementConstraint(elt : AGNode,
                        iss :List[AGNode],
                        frs : List[AGNode]) extends Constraint(elt, iss, frs){

  override def toString =
    "hideElement("+ elt +",\n" +
      interlopers.mkString("[", ",\n", "],\n") +
      friends.mkString("[", ",\n", "]).")
}