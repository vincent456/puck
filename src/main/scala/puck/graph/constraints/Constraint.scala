package puck.graph.constraints

import puck.graph.{NodeKind, AGEdge}

/**
 * Created by lorilan on 03/06/14.
 */

trait Constraint[Kind <: NodeKind[Kind]]{
  val owners : NodeSet[Kind]
  val friends : NodeSet[Kind]
  val predicate : String
}

trait ConstraintWithInterlopers[Kind <: NodeKind[Kind]] extends Constraint[Kind]{
  val interlopers : NodeSet[Kind]

  def isViolatedBy(edge : AGEdge[Kind]): Boolean =
    owners.hasScopeThatContains_*(edge.target) &&
    violated(edge)

  //assert owners.hasScopeThatContains_*(edge.target)
  def violated(edge : AGEdge[Kind]): Boolean =
      interlopers.hasScopeThatContains_*(edge.source) &&
      !friends.hasScopeThatContains_*(edge.source)

}



/*
    hiddenFrom(Element, Interloper) :- hideScope(S, Facades, Interlopers, Friends),
        friends(S,Friends,AllFriends),
        'vContains*'(S,Element),			% Element is in S
        \+ 'gContains*'(Facades,Element),		% Element is not in one of the Facades
        'gContains*'(Interlopers,Interloper),	% Interloper is in one of the Interlopers
        \+ 'gContains*'(AllFriends, Interloper),	% but not in one the Friends
        \+ 'vContains*'(S,Interloper).		% Interloper is not in S
*/

case class ScopeConstraint[Kind <: NodeKind[Kind]](owners : NodeSet[Kind],
                           facades: NodeSet[Kind],
                           interlopers : NodeSet[Kind],
                           friends : NodeSet[Kind]) extends ConstraintWithInterlopers[Kind] {

  val predicate = "hideScopeSet"

  override def toString =
     predicate +  ConstraintPrinter.format(owners, facades, interlopers, friends)

  override def isViolatedBy(edge : AGEdge[Kind])=
    super.isViolatedBy(edge) &&
      !facades.hasScopeThatContains_*(edge.target)

  override def violated(edge : AGEdge[Kind])=
    super.violated(edge) &&
      !facades.hasScopeThatContains_*(edge.target)
}

/*
    hiddenFrom(Element, Interloper) :- hide(Element, Interlopers, Friends),
         friends(Element, Friends, AllFriends),
         'gContains*'(Interlopers, Interloper),
         \+ 'gContains*'(AllFriends, Interloper).

*/

case class ElementConstraint[Kind <: NodeKind[Kind]](owners : NodeSet[Kind],
                        interlopers : NodeSet[Kind],
                        friends : NodeSet[Kind]) extends ConstraintWithInterlopers[Kind]{

  val predicate = "hideElementSet"
  override def toString = {
    val fmtStr =
      if (interlopers.nonEmpty && friends.nonEmpty)
        "(" + owners + ",\n" +
          interlopers.mkString("[", ",\n", "],\n") +
          friends.mkString("[", ",\n", "]).")
      else
        ConstraintPrinter.format(owners, NodeSet.emptySet(),
          interlopers, friends)
    predicate + fmtStr
  }
}

case class FriendConstraint[Kind <: NodeKind[Kind]](friends : NodeSet[Kind],
                       befriended : NodeSet[Kind])
  extends Constraint[Kind]{

  val owners = befriended
  val predicate = "areFriendsOf"
  override def toString =
    predicate + "(" + friends + ", " + befriended +")."
}

object ConstraintPrinter{

  def format[Kind <: NodeKind[Kind]](hidden : NodeSet[Kind],
             facades: NodeSet[Kind],
             interlopers : NodeSet[Kind],
             friends : NodeSet[Kind]) = {

    def twoArgsFormat(constraint : String, set : NodeSet[Kind]) =
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