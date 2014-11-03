package puck.graph.immutable.constraints

import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable.{AccessGraph, NodeSet, NodeKind, AGEdge}

/**
 * Created by lorilan on 03/06/14.
 */

trait Constraint[Kind <: NodeKind[Kind]]{

  type GraphT = AccessGraph[Kind,_]
  val owners : NodeSet[Kind]
  val friends : NodeSet[Kind]
  val predicate : String
  def mkString(graph : GraphT) : String

  def addFriend(friend : NodeId[Kind]) : Constraint[Kind]
}

trait ConstraintWithInterlopers[Kind <: NodeKind[Kind]] extends Constraint[Kind]{
  val interlopers : NodeSet[Kind]

  def isViolatedBy(graph : GraphT, edge : AGEdge[Kind]): Boolean =
    owners.hasScopeThatContains_*(graph, edge.target) &&
    violated(graph, edge)

  //assert owners.hasScopeThatContains_*(edge.target)
  def violated(graph : GraphT, edge : AGEdge[Kind]): Boolean =
      interlopers.hasScopeThatContains_*(graph, edge.source) &&
      !friends.hasScopeThatContains_*(graph, edge.source)

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

  def mkString(graph : GraphT) =
     predicate +  ConstraintPrinter.format(graph, owners, facades, interlopers, friends)

  override def isViolatedBy(graph : GraphT, edge : AGEdge[Kind])=
    super.isViolatedBy(graph, edge) &&
      !facades.hasScopeThatContains_*(graph, edge.target)

  override def violated(graph : GraphT, edge : AGEdge[Kind])=
    super.violated(graph, edge) &&
      !facades.hasScopeThatContains_*(graph, edge.target)

  def addFriend(friend : NodeId[Kind]) = ScopeConstraint(owners, facades, interlopers, friends + friend)
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
  def mkString(graph : GraphT) = {
    val fmtStr =
      if (interlopers.nonEmpty && friends.nonEmpty)
        "(" + owners + ",\n" +
          interlopers.mkString(graph) + ",\n" +
          friends.mkString(graph) + ")."
      else
        ConstraintPrinter.format(graph, owners, NodeSet.emptySet(),
          interlopers, friends)
    predicate + fmtStr
  }

  def addFriend(friend : NodeId[Kind]) = ElementConstraint(owners, interlopers, friends + friend)
}

case class FriendConstraint[Kind <: NodeKind[Kind]](friends : NodeSet[Kind],
                       befriended : NodeSet[Kind])
  extends Constraint[Kind]{

  val owners = befriended
  val predicate = "areFriendsOf"
  def mkString(graph : GraphT) : String =
    predicate + "(" + friends.mkString(graph) + ", " + befriended.mkString(graph) +")."

  def addFriend(friend : NodeId[Kind]) = FriendConstraint(friends + friend, befriended)
}

object ConstraintPrinter{

  def format[Kind <: NodeKind[Kind]]
  ( graph : AccessGraph[Kind, _],
    hidden : NodeSet[Kind],
    facades: NodeSet[Kind],
    interlopers : NodeSet[Kind],
    friends : NodeSet[Kind]) = {

    def twoArgsFormat(constraint : String, set : NodeSet[Kind]) =
      constraint + "(" + hidden + ", " + set + ")."

    (facades.isEmpty, interlopers.isEmpty, friends.isEmpty) match {
      case (true, false, true) =>
        if(graph.isRoot(interlopers.head))
         "(" + hidden + ")."
        else
         twoArgsFormat("From", interlopers)
      case (true, false, false)
        if graph.isRoot(interlopers.head) => twoArgsFormat("ButFrom", friends)
      case (false, false, true)
        if graph.isRoot(interlopers.head) => twoArgsFormat("But", facades)
      case (_, _, _) => "("+ hidden + ",\n" +
        facades + ",\n" +
        interlopers +  ",\n" +
        friends + ")."
    }
  }

}