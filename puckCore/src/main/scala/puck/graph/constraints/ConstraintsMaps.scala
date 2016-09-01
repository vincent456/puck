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

import puck.graph.constraints.ConstraintsMaps._

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
  hideConstraints : HideConstraintMap ) {

  //heavily used by the solver need to be optimized
  def forAncestors(graph : DependencyGraph, nid : NodeId)(f : Range => Boolean): Boolean =
  f(Element(nid)) || {
    var res = false
    var toVisit : Seq[NodeId] = Seq(nid)
    var visited : Set[NodeId] = Set()

    while(!res && toVisit.nonEmpty) {
      val id = toVisit.head
      visited += id
      toVisit = toVisit.tail

      graph getNode id match {
        case ConcreteNode(_,_,_) =>
          res = f(Scope(id))
          graph.edges.containers.get(nid) match {
            case None => ()
            case Some(cid) =>
              if(!(visited contains cid))
                toVisit +:= cid
          }
        case VirtualNode(_, ids, _) =>
          toVisit = (ids diff visited).toSeq ++: toVisit
      }
    }

    res
  }

  def addHideConstraint(ct : Constraint) =
    copy(hideConstraints = addConstraintToMap(hideConstraints, ct))

  def addFriendConstraint(ct : Constraint) =
    copy(friendConstraints = addConstraintToMap(friendConstraints, ct))


  def friendOf(graph : DependencyGraph, node : NodeId, befriended : NodeId) : Boolean =
    forAncestors(graph, befriended){ befriended0 =>
      val frCtSet = friendConstraints.getOrElse(befriended0, ConstraintSet.empty)
      frCtSet.hasFriendRangeThatContains_*(graph, node)
    }


  def interloperOf(graph : DependencyGraph, user : NodeId, used : NodeId) : Boolean =
    forAncestors(graph, used){ used1 =>
      !graph.contains_*(used1.nid, user) &&
        hideConstraints.getOrElse(used1, Iterable.empty).exists(_.forbid(graph, user, used))
    }


  @inline
  def isForbiddenPredicate(graph : DependencyGraph, e : NodeIdP) : Boolean =
    isForbiddenPredicate(graph, e.source, e.target)

  def isForbiddenPredicate(graph : DependencyGraph, user : NodeId, used : NodeId) : Boolean =
    interloperOf(graph, user, used) && !friendOf(graph, user, used)

  def computeForbiddenDependencies(graph : DependencyGraph) : Set[NodeIdP] =
    (graph.usesList filter (isForbiddenPredicate(graph,_))).toSet ++
    (graph.containsList filter (isForbiddenPredicate(graph,_)))




  def checkOrCompute(graph: DependencyGraph) : Set[NodeIdP] = {
    graph.constraintsMapsCache match {
      case Some((cm, s)) if cm eq this => s
      case _ =>
        val s = computeForbiddenDependencies(graph)
        graph.constraintsMapsCache = Some((this, s))
        s

    }
  }
  @inline
  def isForbidden(graph : DependencyGraph, user : NodeId, used : NodeId) : Boolean  =
    isForbidden(graph, (user,used))

  def isForbidden(graph : DependencyGraph, e : NodeIdP) : Boolean =
    checkOrCompute(graph) contains e

  def forbiddenDependencies(graph : DependencyGraph) : Seq[DGEdge] =
    checkOrCompute(graph).toSeq flatMap {
      case (source, target) =>
        (graph.contains(source, target), graph.uses(source, target)) match {
          case (true, false) => Seq(Contains(source, target))
          case (false, true) => Seq(Uses(source, target))
          case (true, true) =>Seq(Contains(source, target), Uses(source, target))
          case (false, false) => puck.error("should not happen")

        }
    }

  def noForbiddenDependencies(graph: DependencyGraph) : Boolean =
    checkOrCompute(graph).isEmpty


  def isWronglyContained(graph : DependencyGraph, node : NodeId) : Boolean =
    graph container node exists (isForbidden(graph, _, node))


  def isWronglyUsed(graph : DependencyGraph, id : NodeId) =
    wrongUsers(graph, id).nonEmpty

  def wrongUsers(graph : DependencyGraph, node : NodeId) : List[NodeId] = {
    graph.usersOf(node).foldLeft(List[NodeId]()){ case(acc, user) =>
      if( isForbidden(graph, user, node) ) user +: acc
      else acc
    }
  }

  // ! \\ do not change hideFrom
  def addHideFromRootException(graph : DependencyGraph, node : NodeId, friend : NodeId): ConstraintsMaps ={

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
