package puck.graph.constraints

import puck.graph.AGError

/**
 * Created by lorilan on 03/06/14.
 */

abstract class Constraint(val owners : NodeSet,
                          val friends : NodeSet)

class ScopeConstraint(scope : NodeSet,
                      val facades: NodeSet,
                      val interlopers : NodeSet,
                      frs : NodeSet) extends Constraint(scope, frs){

  override def toString =
    "hideScopeSet" +  ConstraintPrinter.format(owners, facades, interlopers, friends)

}

class ElementConstraint(elt : NodeSet,
                        val interlopers : NodeSet,
                        frs : NodeSet) extends Constraint(elt, frs){

  override def toString = {
    val fmtStr =
      if (!interlopers.isEmpty && !friends.isEmpty)
        "(" + elt + ",\n" +
          interlopers.mkString("[", ",\n", "],\n") +
          friends.mkString("[", ",\n", "]).")
      else
        ConstraintPrinter.format(owners, NodeSet.emptySet(),
          interlopers, friends)
    "hideElementSet" + fmtStr
  }
}

class FriendConstraint(frds : NodeSet,
                       befriended : NodeSet) 
  extends Constraint(befriended, frds){
  override def toString =
    "areFriendsOf(" + frds + ", " + befriended +")."
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
      case (true, true, false) => twoArgsFormat("ButFrom", friends)
      case (false, true, true) => twoArgsFormat("But", facades)
      case (false, false, false) => "("+ hidden + ",\n" +
        facades + ",\n" +
        interlopers +  ",\n" +
        friends + ")."
      case _ => throw new AGError("ConstraintPrinter.format unhandled case !")
    }


  }

}