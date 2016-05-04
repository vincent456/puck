/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.graph
package constraints

import puck.graph.ShowDG._
import puck.graph.constraints.ConstraintsMaps._
import puck.util.Logger



object ConstraintsMaps{
  type FriendConstraintMap = Map[Range, ConstraintSet]
  type HideConstraintMap = Map [Range, ConstraintSet]

  def apply() = new ConstraintsMaps(Map(), Map(), Map())

  def addConstraintToMap(map : Map [Range, ConstraintSet], ct : Constraint) = {
    ct.owners.foldLeft(map){
      case (m, owner) =>
        val s = m.getOrElse(owner, new ConstraintSet())
        m + (owner -> (s + ct) )
    }
  }
}

case class ConstraintsMaps
( namedSets : Map[String, NamedRangeSet],
  friendConstraints : FriendConstraintMap,
  hideConstraints : HideConstraintMap )
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

   def addHideConstraint(ct : Constraint) =
     copy(hideConstraints = addConstraintToMap(hideConstraints, ct))

   def addFriendConstraint(ct : Constraint) =
     copy(friendConstraints = addConstraintToMap(friendConstraints, ct))


   def printConstraints[V](graph : GraphT, logger : Logger[V])(implicit v : V) : Unit = {
     logger writeln "printing constraints"
     namedSets.foreach{
       case (_, namedSet) => logger.writeln((graph, namedSet).shows(stringOfNamedRangeSetDef))
     }

     def printMap( m : Map [Range, ConstraintSet], cb : DGStringBuilder[Constraint]) =
       m foreach { case (k, s) =>
         s.foreach { c => if(c.owners.head == k )
           logger.writeln((graph, c).shows(cb))(v)}
       }
     printMap(hideConstraints, ShowDG.stringOfConstraint)
     printMap(friendConstraints, ShowDG.stringOfFriendConstraint)
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
     val uses = Uses(user, used)
      forAncestors(graph, used){ used1 =>
       !graph.contains_*(used1.nid, user) &&
         hideConstraints.getOrElse(used1, Iterable.empty).exists(_.violated(graph, uses))
     }
   }

   def isViolation(graph : GraphT, user : NIdT, used : NIdT) =
     interloperOf(graph, user, used) && !friendOf(graph, user, used)


   def isWronglyContained(graph : GraphT, node : NIdT) : Boolean =
    graph container node map (isViolation(graph, _, node)) getOrElse false


   def wrongUsers(graph : GraphT, node : NIdT) : List[NIdT] = {
     graph.usersOf(node).foldLeft(List[NIdT]()){ case(acc, user) =>
       if( isViolation(graph, user, node) ) user +: acc
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
     ???
   }

}
