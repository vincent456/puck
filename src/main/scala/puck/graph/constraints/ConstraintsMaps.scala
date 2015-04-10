package puck.graph
package constraints

import puck.util.Logger

import scalaz.Show

/**
 * Created by lorilan on 31/10/14.
 */
object ConstraintsMaps{
  type FriendConstraintMap = Map[Range, ConstraintSet]
  type HideConstraintMap = Map [Range, ConstraintSet]

  def apply() = new ConstraintsMaps(Map(), Map(), Map())
}

import puck.graph.constraints.ConstraintsMaps._

case class ConstraintsMaps
(namedSets : Map[String, NamedRangeSet],
 friendConstraints : FriendConstraintMap,
 hideConstraints : HideConstraintMap)
 {

   type GraphT = DependencyGraph
   type NIdT = NodeId

   def forAncestors(graph : GraphT, nid : NodeId)(f : Range => Boolean): Boolean ={

     def aux(nid : NodeId) : Boolean =
       f(Scope(nid)) || (graph.container(nid) match {
       case None => false
       case Some(id) => aux(id)
     })
     f(Element(nid)) || aux(nid)
   }




   def forContainer(graph : GraphT, nid : NodeId)(f : NodeId => Boolean): Boolean =
     graph.container(nid) match {
     case None => false
     case Some(id) => f(id)
   }


   def printConstraints[V](graph : GraphT, logger : Logger[V], v : V) : Unit = {
     import ShowConstraints._
     import ShowDG.{showDG, graphToOptionGraph}
     namedSets.foreach{
       case (_, namedSet) => logger.writeln(showDG(graph)(namedRangeSetDefCord).show(namedSet))(v)
     }
     def printMap( m : Map [Range, ConstraintSet]) =
       m foreach { case (k, s) =>
         s.foreach { c => if(c.owners.head == k )
           logger.writeln(showDG[Constraint](graph).shows(c))(v)}
       }
     printMap(hideConstraints)
     printMap(friendConstraints)
   }


   def friendOf(graph : GraphT, node : NIdT, befriended : NIdT) : Boolean = {
     forAncestors(graph, befriended){ befriended0 =>
       val frCtSet = friendConstraints.getOrElse(befriended0, ConstraintSet.empty)
       frCtSet.hasFriendRangeThatContains_*(graph, node)
     }
   }

   /*def violatedScopeConstraintsOf(graph : GraphT, user : NIdT, usee0 : NIdT) : Seq[Constraint] = {
     val uses = DGEdge.uses(user, usee0)

     def aux(usee : NIdT, acc : Seq[Constraint]) : Seq[Constraint] = {
       val acc2 = if(!graph.contains_*(usee, user))
         hideConstraints.getOrElse(usee, Iterable.empty).filter(_.violated(graph, uses)) ++: acc
       else acc

       graph.container(usee) match {
         case None => acc2
         case Some(cterId) => aux(cterId, acc2)
       }
     }
     aux(usee0,List())
   }*/

   def interloperOf(graph : GraphT, user : NIdT, used : NIdT) : Boolean = {
     val uses = DGEdge.uses(user, used)
      forAncestors(graph, used){ used1 =>
       !graph.contains_*(used1.nid, user) &&
         hideConstraints.getOrElse(used1, Iterable.empty).exists(_.violated(graph, uses))
     }

   }

   def violation(graph : GraphT, user : NIdT, used : NIdT) =
     interloperOf(graph, user, used) && !friendOf(graph, user, used)


   def isWronglyContained(graph : GraphT, node : NIdT) : Boolean =
    forContainer(graph, node)(violation(graph, _, node))


   def wrongUsers(graph : GraphT, node : NIdT) : Seq[NIdT] = {
     graph.usersOf(node).foldLeft(Seq[NIdT]()){ case(acc, user) =>
       if( violation(graph, user, node) ) user +: acc
       else acc
     }
   }

   // ! \\ do not change hideFrom
   def addHideFromRootException(graph : GraphT, node : NIdT, friend : NIdT): ConstraintsMaps ={

     def aux(range : Range, constraintsMap : Map [Range, ConstraintSet]) :
     Map [Range, ConstraintSet] = {
       val nodeConstraintsSet = constraintsMap.getOrElse(range, ConstraintSet.empty)
       if(nodeConstraintsSet.isEmpty) constraintsMap
       else {
         val (newCts, changedMap, allOwners) =
           nodeConstraintsSet.foldLeft(Seq[Constraint](), Seq[(Constraint, Constraint)](), Seq[Range]()) {
             case ((acc, map, owners), ct) =>
               if (ct.interlopers hasRangeThatContains_* (graph, DependencyGraph.rootId) ){
                 ct.friends match {
                   case NamedRangeSet(name, definition) => ???
                   case _ =>
                     val newCt = ct.addFriend(Element(friend)) // /!\
                     (newCt +: acc, (ct, newCt) +: map, owners ++ ct.owners.iterator)
                 }
               }
               else (ct +: acc, map, owners)
           }

        val newConstraintsMap = constraintsMap + (range -> new ConstraintSet(newCts))

         allOwners.foldLeft(newConstraintsMap){
            case (m2, id) if id != range =>
                 m2 + (id -> m2(id).replaceEq(changedMap))
            case (m2, id) => m2
           }
         }
       }


     this.copy(hideConstraints = aux(Element(node), aux(Scope(node), hideConstraints)))

   }

}
