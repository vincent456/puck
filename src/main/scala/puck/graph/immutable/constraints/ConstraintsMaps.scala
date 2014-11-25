package puck.graph.immutable.constraints

import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable.{NamedNodeSet, AGEdge, AccessGraph, NodeKind}
import puck.util.Logger

/**
 * Created by lorilan on 31/10/14.
 */
object ConstraintsMaps{
  type CanSeeMap = Map[NodeId, CanSeeSet]
  type FriendsOfScopesMap = Map[NodeId, ConstraintSet[ScopeFriendOfScopesConstraint]]
  type FriendsOfElementsMap = Map[NodeId, ConstraintSet[ScopeFriendOfElementsConstraint]]
  type CtWithInterlopersMap[CtT <: ConstraintWithInterlopers] = Map [NodeId, ConstraintSet[CtT]]

  type EltConstraintsMap = Map [NodeId, ConstraintSet[ElementConstraint]]
  type ScopeConstraintsMap = Map [NodeId, ConstraintSet[ScopeConstraint]]

  def apply() = new ConstraintsMaps(Map(), Map(), Map(), Map(), Map(), Map())
}

import ConstraintsMaps._

class ConstraintsMaps
(val namedSets : Map[String, NamedNodeSet],
 val friendOfElementsConstraints : FriendsOfElementsMap,
 val friendOfScopesConstraints : FriendsOfScopesMap,
 val canSeeConstraints : CanSeeMap,
 val elementsConstraints : EltConstraintsMap,
 val scopeConstraints : ScopeConstraintsMap)
 {

   def newConstraintsMaps( nNamedSets : Map[String, NamedNodeSet] = namedSets,
                           nFriendsOfElementsConstraints : FriendsOfElementsMap = friendOfElementsConstraints,
                           nFriendOfScopesConstraints : FriendsOfScopesMap = friendOfScopesConstraints,
                           nCanSeeConstraints : CanSeeMap = canSeeConstraints,
                           nElementsConstraints : EltConstraintsMap = elementsConstraints,
                           nScopeConstraints : ScopeConstraintsMap = scopeConstraints) =
    new ConstraintsMaps(nNamedSets,
                        nFriendsOfElementsConstraints,
                        nFriendOfScopesConstraints,
                        nCanSeeConstraints,
                        nElementsConstraints,
                        nScopeConstraints)

   type GraphT = AccessGraph
   type NIdT = NodeId

   def forAncestors(graph : GraphT, nid : NodeId)(f : NodeId => Boolean): Boolean =
     f(nid) || (graph.container(nid) match {
      case None => false
      case Some(id) => forAncestors(graph, id)(f)
     })

   def forContainer(graph : GraphT, nid : NodeId)(f : NodeId => Boolean): Boolean =
     graph.container(nid) match {
     case None => false
     case Some(id) => f(id)
   }


   def printConstraints[V](graph : GraphT, logger : Logger[V], v : V){
     namedSets.foreach{
       case (_, namedSet) => logger.writeln(namedSet.mkDefString(graph))(v)
     }
     type CtMap[CtT <: Constraint] = Map [NodeId, ConstraintSet[CtT]]
     def printMap[CtT <: Constraint]( m : CtMap[CtT]) = m foreach { case (k, s) =>
         s.foreach { s => if(s.owners.head == k) logger.writeln(s.mkString(graph))(v)}
     }
     printMap(canSeeConstraints)
     printMap(friendOfElementsConstraints)
     printMap(friendOfScopesConstraints)
     printMap(elementsConstraints)
     printMap(scopeConstraints)
   }


   def friendOfScope(graph : GraphT, node : NIdT, befriended : NIdT) : Boolean = {
     forAncestors(graph, befriended){ befriended0 =>
       val frCtSet = friendOfScopesConstraints.getOrElse(befriended0, ConstraintSet.empty)
       frCtSet.hasFriendScopeThatContains_*(graph, node)
     }
   }
   def friendOfElement(graph : GraphT, node : NIdT, befriended : NIdT) : Boolean = {
     val frCtSet = friendOfElementsConstraints.getOrElse(befriended, ConstraintSet.empty)
     frCtSet.hasFriendScopeThatContains_*(graph, node)
   }

   def eltFriendOfElement(graph: GraphT, node : NIdT, befriended : NIdT) : Boolean = {
     val frCtSet = canSeeConstraints.getOrElse(befriended, CanSeeSet())
     frCtSet.isFriend(node)
   }


   def violatedScopeConstraintsOf(graph : GraphT, user : NIdT, usee0 : NIdT) : Seq[ScopeConstraint] = {
     val uses = AGEdge.uses(user, usee0)

     def aux(usee : NIdT, acc : Seq[ScopeConstraint]) : Seq[ScopeConstraint] = {
       val acc2 = if(!graph.contains_*(usee, user))
         scopeConstraints.getOrElse(usee, Iterable.empty).filter(_.violated(graph, uses)) ++: acc
       else acc

       graph.container(usee) match {
         case None => acc2
         case Some(cterId) => aux(cterId, acc2)
       }
     }
     aux(usee0,List())
   }


   def potentialScopeInterloperOf(graph : GraphT, user : NIdT, usee0 : NIdT) : Boolean = {
     val uses = AGEdge.uses(user, usee0)

     def aux(usee: NIdT): Boolean =
       forAncestors(graph, usee){ usee1 =>
         !graph.contains_*(usee1, user) &&
           scopeConstraints.getOrElse(usee1, Iterable.empty).exists(_.violated(graph, uses))
       }

     aux(usee0)
   }

   def violatedElementConstraintOf(graph : GraphT, user : NIdT, usee: NIdT) =
     elementsConstraints.getOrElse(usee, Iterable.empty).filter(_.violated(graph, AGEdge.uses(user, usee)))

   def potentialElementInterloperOf(graph : GraphT, user : NIdT, usee: NIdT) : Boolean =
     elementsConstraints.getOrElse(usee, Iterable.empty).exists(_.violated(graph, AGEdge.uses(user, usee)))

   def interloperOf(graph : GraphT, user : NIdT, usee : NIdT) =
     (potentialScopeInterloperOf(graph, user, usee)
       || potentialElementInterloperOf(graph, user, usee)) &&
       !(friendOfElement(graph, user, usee) ||
         friendOfScope(graph, user, usee) ||
         eltFriendOfElement(graph, user, usee))

   def isWronglyContained(graph : GraphT, node : NIdT) : Boolean =
    forContainer(graph, node)(interloperOf(graph, _, node))


   def wrongUsers(graph : GraphT, node : NIdT) : Seq[NIdT] = {
     graph.users(node).foldLeft(Seq[NIdT]()){ case(acc, user) =>
       if( interloperOf(graph, user, node) ) user +: acc
       else acc
     }
   }

   // ! \\ do not change hideFrom
   def addHideFromRootException(node : NIdT, friend : NIdT): ConstraintsMaps ={

     def aux[CtT <: ConstraintWithInterlopers](constraintsMap : Map [NodeId, ConstraintSet[CtT]]) :
     Map [NodeId, ConstraintSet[CtT]] = {
       val nodeConstraintsSet = constraintsMap.getOrElse(node, ConstraintSet.empty)
       if(nodeConstraintsSet.isEmpty) constraintsMap
       else {
         val (newCts, changedMap, allOwners) =
           nodeConstraintsSet.foldLeft(Seq[CtT](), Seq[(CtT, CtT)](), Seq[NIdT]()) {
             case ((acc, map, owners), ct) =>
               if (ct.interlopers contains AccessGraph.rootId){
                 ct.scopeFriends match {
                   case NamedNodeSet(name, definition) => ???
                   case _ =>
                     val newCt = ct.addFriend(friend).asInstanceOf[CtT] // /!\
                     (newCt +: acc, (ct, newCt) +: map, owners ++ ct.owners.iterator)
                 }
               }
               else (ct +: acc, map, owners)
           }

        val newConstraintsMap = constraintsMap + (node -> new ConstraintSet[CtT](newCts))

         allOwners.foldLeft(newConstraintsMap){
            case (m2, id) if id != node =>
                 m2 + (id -> m2(id).replaceEq(changedMap))
            case (m2, id) => m2
           }
         }
       }


     newConstraintsMaps(nScopeConstraints = aux(scopeConstraints),
                        nElementsConstraints = aux(elementsConstraints))

   }

}
