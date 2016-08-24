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

package puck.graph.io

import puck.graph.constraints.ConstraintsMaps
import puck.graph.{NodeKind, DependencyGraph, NodeId}

sealed abstract class Visibility{
  def opposite : Visibility
}
case object Hidden extends Visibility {
  def opposite = Visible
}
case object Visible extends Visibility {
  def opposite = Hidden
}

object VisibilitySet{

  type T = Set[NodeId]

  def allVisible(graph : DependencyGraph) : T = Set[NodeId]()


  def allHidden(graph : DependencyGraph) : T =
    Set[NodeId]().setVisibility(graph.nodesId, Hidden)


  def violationsOnly(graph : DependencyGraph, constraints : ConstraintsMaps) : T =
    (graph, constraints).violations.foldLeft(allHidden(graph)){
      (s, edge) =>
      val path1 = graph.containerPath(edge.source)
      val path2 = graph.containerPath(edge.target)

      s.setVisibility(path1, Visible)
        .setVisibility(path2, Visible)
    }
  
  def visibleKinds(graph : DependencyGraph, kinds : Set[NodeKind]) = {
    val visibleNodes = graph.nodes filter (kinds contains _.kind) map (_.id)
    allHidden(graph).setVisibility(visibleNodes, Visible)
  }


  def topLevelVisible(graph : DependencyGraph) : T =
    allHidden(graph).setVisibility(graph.content(DependencyGraph.rootId), Visible)


  def hideWithName
  ( graph : DependencyGraph, visible : T,
    nodeId : NodeId, name : Seq[String]) : T =
    name match {
      case Nil => visible
      case hd +: tl =>
        graph.content(nodeId) find (graph.getConcreteNode(_).name == hd) match {
          case None => visible
          case Some(nid) =>
            hideWithName(graph, visible.setVisibility(nid, Hidden), nid, tl)
        }
    }

  implicit class VisibilitySetOps(val hiddens: T) extends AnyVal {

    def hideWithName(graph : DependencyGraph, name : Seq[String]) : T =
      VisibilitySet.hideWithName(graph, hiddens, graph.rootId, name)

    def visibleNodes(graph : DependencyGraph) : Set[NodeId] =
      graph.nodesId.toSet -- hiddens

    def setVisibility(id : NodeId, v : Visibility) : T = v match {
      case Visible => hiddens - id
      case Hidden => hiddens + id
    }

    def setVisibility(ids : Iterable[NodeId], v : Visibility) : T = v match {
      case Visible => hiddens -- ids
      case Hidden => hiddens ++ ids
    }

    def toggle(id : NodeId): T =
      setVisibility(id, visibility(id).opposite)


    def isVisible : NodeId => Boolean = id => !isHidden(id)
    def isHidden : NodeId => Boolean = hiddens.contains

    def visibility(id : NodeId) : Visibility =
      if(isVisible(id)) Visible
      else Hidden

  }

}

