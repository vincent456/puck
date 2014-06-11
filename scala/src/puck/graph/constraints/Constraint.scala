package puck.graph.constraints

import puck.graph.{AGEdge, AGError}

/**
 * Created by lorilan on 03/06/14.
 */

trait ConstraintWithInterlopers {
  val owners : NodeSet
  val friends : NodeSet
  val interlopers : NodeSet

  def isViolatedBy(edge : AGEdge): Boolean =
    owners.hasScopeThatContains_*(edge.target) &&
    interlopers.hasScopeThatContains_*(edge.source) &&
    !friends.hasScopeThatContains_*(edge.target)

}

abstract class Constraint{
  val owners : NodeSet
  val friends : NodeSet
  val predicate : String
}

case class ScopeConstraint(owners : NodeSet,
                           facades: NodeSet,
                           interlopers : NodeSet,
                           friends : NodeSet) extends Constraint with ConstraintWithInterlopers{

  val predicate = "hideScopeSet"

  override def toString =
     predicate +  ConstraintPrinter.format(owners, facades, interlopers, friends)

  override def isViolatedBy(edge : AGEdge)=
    super.isViolatedBy(edge) &&
      !facades.hasScopeThatContains_*(edge.target)

}

case class ElementConstraint(owners : NodeSet,
                        interlopers : NodeSet,
                        friends : NodeSet) extends Constraint with ConstraintWithInterlopers{

  val predicate = "hideElementSet"
  override def toString = {
    val fmtStr =
      if (!interlopers.isEmpty && !friends.isEmpty)
        "(" + owners + ",\n" +
          interlopers.mkString("[", ",\n", "],\n") +
          friends.mkString("[", ",\n", "]).")
      else
        ConstraintPrinter.format(owners, NodeSet.emptySet(),
          interlopers, friends)
    predicate + fmtStr
  }
}

case class FriendConstraint(friends : NodeSet,
                       befriended : NodeSet)
  extends Constraint{

  val owners = befriended
  val predicate = "areFriendsOf"
  override def toString =
    predicate + "(" + friends + ", " + befriended +")."
}

object ConstraintPrinter{

  def format(hidden : NodeSet,
             facades: NodeSet,
             interlopers : NodeSet,
             friends : NodeSet) = {

    def twoArgsFormat(constraint : String, set : NodeSet) =
      constraint + "(" + hidden + ", " + set + ")."

    (facades.isEmpty, interlopers.isEmpty, friends.isEmpty) match {
      case (true, false, true) =>
        if(interlopers.head  == interlopers.head.graph.root)
         "(" + hidden + ")."
        else
         twoArgsFormat("From", interlopers)
      case (true, false, false)
        if interlopers.head  == interlopers.head.graph.root => twoArgsFormat("ButFrom", friends)
      case (false, false, true)
        if interlopers.head  == interlopers.head.graph.root => twoArgsFormat("But", facades)
      case (_, _, _) => "("+ hidden + ",\n" +
        facades + ",\n" +
        interlopers +  ",\n" +
        friends + ")."
    }
  }

}