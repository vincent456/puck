package puck.graph.immutable.constraints

import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable.{NamedNodeSet, AGEdge, AccessGraph, NodeKind}
import puck.util.Logger

/**
 * Created by lorilan on 31/10/14.
 */
object ConstraintsMaps{
  type FriendsMap[Kind <: NodeKind[Kind]] = Map[NodeId[Kind], ConstraintSet[Kind, FriendConstraint[Kind]]]
  type CtWithInterlopersMap[Kind <: NodeKind[Kind], CtT <: ConstraintWithInterlopers[Kind]] =
                  Map [NodeId[Kind], ConstraintSet[Kind, CtT]]


  type EltConstraintsMap[Kind <: NodeKind[Kind]] = Map [NodeId[Kind], ConstraintSet[Kind, ElementConstraint[Kind]]]
  type ScopeConstraintsMap[Kind <: NodeKind[Kind]] = Map [NodeId[Kind], ConstraintSet[Kind, ScopeConstraint[Kind]]]

  def apply[Kind  <: NodeKind[Kind]]() = new ConstraintsMaps[Kind](Map(), Map(), Map(), Map())
}

import ConstraintsMaps._

class ConstraintsMaps[Kind <: NodeKind[Kind]]
(val nodeSets : Map[String, NamedNodeSet[Kind]],
 val friendConstraints : FriendsMap[Kind],
 val elementsConstraints : EltConstraintsMap[Kind],
 val scopeConstraints : ScopeConstraintsMap[Kind])
 {

   def newConstraintsMaps( nNodeSets : Map[String, NamedNodeSet[Kind]] = nodeSets,
                           nFriendConstraints : FriendsMap[Kind] = friendConstraints,
   nElementsConstraints : EltConstraintsMap[Kind] = elementsConstraints,
   nScopeConstraints : ScopeConstraintsMap[Kind] = scopeConstraints) =
    new ConstraintsMaps(nNodeSets,
                        nFriendConstraints,
                        nElementsConstraints,
                        nScopeConstraints)

   type GraphT = AccessGraph[Kind, _]
   type NIdT = NodeId[Kind]


   def printConstraints[V](graph : GraphT, logger : Logger[V], v : V){
     nodeSets.foreach{
       case (_, namedSet) => logger.writeln(namedSet.mkString(graph))(v)
     }
     type CtMap[CtT <: Constraint[Kind]] = Map [NodeId[Kind], ConstraintSet[Kind, CtT]]
     def printMap[CtT <: Constraint[Kind]]( m : CtMap[CtT]) = m foreach { case (k, s) =>
         s.foreach { s => if(s.owners.head == k) logger.writeln(s.mkString(graph))(v)}
     }
     printMap(scopeConstraints)
     printMap(friendConstraints)
     printMap(elementsConstraints)
   }


   def friendOf(graph : GraphT, node : NIdT, befriended : NIdT) : Boolean = {
     val frCtSet = friendConstraints.getOrElse(befriended, ConstraintSet.empty)
     frCtSet.hasFriendScopeThatContains_*(graph, node) ||
       !graph.isRoot(befriended) && friendOf(graph, node, graph.container(befriended))
   }

   def violatedScopeConstraintsOf(graph : GraphT, user : NIdT, usee0 : NIdT) : Seq[ScopeConstraint[Kind]] = {
     val uses = AGEdge.uses[Kind](user, usee0)

     def aux(usee : NIdT, acc : Seq[ScopeConstraint[Kind]]) : Seq[ScopeConstraint[Kind]] = {
       val acc2 = if(!graph.contains_*(usee, user))
         scopeConstraints.getOrElse(usee, Iterable.empty).filter(_.violated(graph, uses)) ++: acc
       else acc

       if(graph.isRoot(usee)) acc2
       else aux(graph.container(usee), acc2)
     }
     aux(usee0,List())
   }


   def potentialScopeInterloperOf(graph : GraphT, user : NIdT, usee0 : NIdT) : Boolean = {
     val uses = AGEdge.uses[Kind](user, usee0)

     def aux(usee: NIdT): Boolean =
       !graph.contains_*(usee, user) &&
         scopeConstraints.getOrElse(usee, Iterable.empty).exists(_.violated(graph, uses)) ||
         !graph.isRoot(usee) && aux(graph.container(usee))

     aux(usee0)
   }

   def violatedElementConstraintOf(graph : GraphT, user : NIdT, usee: NIdT) =
     elementsConstraints.getOrElse(usee, Iterable.empty).filter(_.violated(graph, AGEdge.uses(user, usee)))

   def potentialElementInterloperOf(graph : GraphT, user : NIdT, usee: NIdT) : Boolean =
     elementsConstraints.getOrElse(usee, Iterable.empty).exists(_.violated(graph, AGEdge.uses(user, usee)))

   def interloperOf(graph : GraphT, user : NIdT, usee : NIdT) =
     (potentialScopeInterloperOf(graph, user, usee)
       || potentialElementInterloperOf(graph, user, usee)) &&
       !friendOf(graph, user, usee)

   def isWronglyContained(graph : GraphT, node : NIdT) : Boolean =
     !graph.isRoot(node) && interloperOf(graph, graph.container(node), node)

   def wrongUsers(graph : GraphT, node : NIdT) : Seq[NIdT] = {
     graph.users(node).foldLeft(Seq[NIdT]()){ case(acc, user) =>
       if( interloperOf(graph, user, node) ) user +: acc
       else acc
     }
   }

   // ! \\ do not change hideFrom
   def addHideFromRootException(node : NIdT, friend : NIdT): ConstraintsMaps[Kind] ={

     def aux[CtT <: ConstraintWithInterlopers[Kind]](constraintsMap : Map [NodeId[Kind], ConstraintSet[Kind, CtT]]) :
     Map [NodeId[Kind], ConstraintSet[Kind, CtT]] = {
       val nodeConstraintsSet = constraintsMap.getOrElse(node, ConstraintSet.empty)
       if(nodeConstraintsSet.isEmpty) constraintsMap
       else {
         val (newCts, changedMap, allOwners) =
           nodeConstraintsSet.foldLeft(Seq[CtT](), Seq[(CtT, CtT)](), Seq[NIdT]()) {
             case ((acc, map, owners), ct) =>
               if (ct.interlopers contains AccessGraph.rootId){
                 ct.friends match {
                   case NamedNodeSet(name, definition) => ???
                   case _ =>
                     val newCt = ct.addFriend(friend).asInstanceOf[CtT] // /!\
                     (newCt +: acc, (ct, newCt) +: map, owners ++ ct.owners.iterator)
                 }
               }
               else (ct +: acc, map, owners)
           }

        val newConstraintsMap = constraintsMap + (node -> new ConstraintSet[Kind, CtT](newCts))

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
