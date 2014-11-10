package puck.graph.immutable.constraints

import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable._

/**
 * Created by lorilan on 03/06/14.
 */

trait Constraint{

  type GraphT = AccessGraph
  val owners : NodeSet
  val scopeFriends : NodeSet
  val predicate : String
  def mkString(graph : GraphT) : String

  def addFriend(friend : NodeId) : Constraint
}

trait ConstraintWithInterlopers extends Constraint{
  val interlopers : NodeSet

  def isViolatedBy(graph : GraphT, edge : AGEdge): Boolean =
    owners.hasScopeThatContains_*(graph, edge.target) &&
    violated(graph, edge)

  //assert owners.hasScopeThatContains_*(edge.target)
  def violated(graph : GraphT, edge : AGEdge): Boolean =
      interlopers.hasScopeThatContains_*(graph, edge.source) &&
      !scopeFriends.hasScopeThatContains_*(graph, edge.source)

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

case class ScopeConstraint(owners : NodeSet,
                           facades: NodeSet,
                           interlopers : NodeSet,
                           scopeFriends : NodeSet) extends ConstraintWithInterlopers {

  val predicate = "hideScopeSet"

  def mkString(graph : GraphT) =
     predicate +  ConstraintPrinter.format(graph, owners, facades, interlopers, scopeFriends)

  override def isViolatedBy(graph : GraphT, edge : AGEdge)=
    super.isViolatedBy(graph, edge) &&
      !facades.hasScopeThatContains_*(graph, edge.target)

  override def violated(graph : GraphT, edge : AGEdge)=
    super.violated(graph, edge) &&
      !facades.hasScopeThatContains_*(graph, edge.target)

  def addFriend(friend : NodeId) = ScopeConstraint(owners, facades, interlopers, scopeFriends + friend)
}

/*
    hiddenFrom(Element, Interloper) :- hide(Element, Interlopers, Friends),
         friends(Element, Friends, AllFriends),
         'gContains*'(Interlopers, Interloper),
         \+ 'gContains*'(AllFriends, Interloper).

*/

case class ElementConstraint(owners : NodeSet,
                        interlopers : NodeSet,
                        scopeFriends : NodeSet) extends ConstraintWithInterlopers{

  val predicate = "hideElementSet"
  def mkString(graph : GraphT) = {
    val fmtStr =
      if (interlopers.nonEmpty && scopeFriends.nonEmpty)
        "(" + owners + ",\n" +
          interlopers.mkString(graph) + ",\n" +
          scopeFriends.mkString(graph) + ")."
      else
        ConstraintPrinter.format(graph, owners, NodeSet.emptySet(),
          interlopers, scopeFriends)
    predicate + fmtStr
  }

  def addFriend(friend : NodeId) = ElementConstraint(owners, interlopers, scopeFriends + friend)
}

case class ScopeFriendOfScopesConstraint(scopeFriends : NodeSet,
                                         befriendedScopes : NodeSet)
  extends Constraint{

  val owners = befriendedScopes
  val predicate = "friendOfScope"
  def mkString(graph : GraphT) : String =
    predicate + "(" + scopeFriends.mkString(graph) + ", " + befriendedScopes.mkString(graph) +")."

  def addFriend(friend : NodeId) = ScopeFriendOfScopesConstraint(scopeFriends + friend, befriendedScopes)
}

case class ScopeFriendOfElementsConstraint(scopeFriends : NodeSet,
                                          befriendedElements : NodeSet)
 extends Constraint {
  val owners = befriendedElements
  val predicate = "friendOfElement"
  def mkString(graph : GraphT) : String =
    predicate + "(" + scopeFriends.mkString(graph) + ", " + befriendedElements.mkString(graph) +")."

  def addFriend(friend : NodeId) = ScopeFriendOfElementsConstraint(scopeFriends + friend, befriendedElements)
}

case class ElementFriendOfElementsConstraint
(elementFriends : NodeSet,
 befriendedElements : NodeSet)
  extends Constraint {

  def mkString(graph : GraphT) : String =
      predicate + "(" + elementFriends.mkString(graph) + ", " + befriendedElements.mkString(graph) +")."

  override val owners: NodeSet = befriendedElements
  override val predicate: String = "canSee"

  override val scopeFriends: NodeSet = LiteralNodeSet()

  override def addFriend(friend: NodeId): Constraint = ???
}

object ConstraintPrinter{

  def format[Kind <: NodeKind]
  ( graph : AccessGraph,
    hidden : NodeSet,
    facades: NodeSet,
    interlopers : NodeSet,
    friends : NodeSet) = {

    def twoArgsFormat(constraint : String, set : NodeSet) =
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