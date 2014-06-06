package puck.graph.constraints

import puck.graph.AGNode

/**
 * Created by lorilan on 03/06/14.
 */

object ConstraintPrinter{

  def format(hidden : AGNode,
             facades: List[AGNode],
             interlopers : List[AGNode],
             friends : List[AGNode]) = {

    def twoArgsFormat(constraint : String, list : List[AGNode]) =
      constraint + "(" + hidden + ", " +
      list.mkString("[", ",\n", "])")

    (facades, interlopers, friends) match {
      case (List(), hidden.graph.root :: List(), List()) =>
        "(" + hidden + ")"
      case (List(), is, List()) => twoArgsFormat("From", is)
      case (List(), List(), frds) => twoArgsFormat("ButFrom", frds)
      case (fcds, List(), List()) => twoArgsFormat("But", fcds)
      case (fcds, is, frds) => "("+ hidden + ",\n" +
        fcds.mkString("[", ",\n", "],\n") +
        is.mkString("[", ",\n", "],\n") +
        frds.mkString("[", ",\n", "]).")
    }


  }


  //def butFrom
  //def but

}

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
    "hideScope" +  ConstraintPrinter.format(owner, facades, interlopers, friends)

}

class ElementConstraint(elt : AGNode,
                        iss :List[AGNode],
                        frs : List[AGNode]) extends Constraint(elt, iss, frs){

  override def toString = {
    val fmtStr =
      if (!interlopers.isEmpty && !friends.isEmpty)
        "(" + elt + ",\n" +
          interlopers.mkString("[", ",\n", "],\n") +
          friends.mkString("[", ",\n", "]).")
      else
        ConstraintPrinter.format(owner, List(), interlopers, friends)
    "hideElement" + fmtStr
  }
}