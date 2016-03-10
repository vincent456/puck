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

package puck
package gui.explorer

import javax.swing.event.{TreeModelEvent, TreeModelListener}
import javax.swing.tree.{TreePath, TreeModel}

import puck.graph._

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Loïc Girault on 16/12/15.
  */


trait TreeModelAdapter extends TreeModel {
  def graph : DependencyGraph
  def getRoot: AnyRef = graph.root

  private [this] val listeners = new ArrayBuffer[TreeModelListener]()

  private def getChildren(parent: scala.Any) : List[DGNode] =
    parent match {
      case node : DGNode => TreeModelAdapter.getChildren(graph, node.id)
      case _ => error(s"expected DGNode got ${parent.getClass}")
    }

  def getIndexOfChild(parent: scala.Any, child: scala.Any): Int =
    getChildren(parent) indexOf child

  def getChildCount(parent: scala.Any): Int =
    getChildren(parent).size

  def getChild(parent: scala.Any, index: Int): DGNode =
    getChildren(parent)(index)

  def isLeaf(node: scala.Any): Boolean =
    getChildCount(node) == 0

  def valueForPathChanged(path: TreePath, newValue: scala.Any): Unit = ()

  def addTreeModelListener(l: TreeModelListener): Unit =
    ignore(listeners += l)

  def removeTreeModelListener(l: TreeModelListener): Unit =
    ignore(listeners -= l)

  def fireNodesChanged(e : TreeModelEvent) =
   listeners foreach (_.treeNodesChanged(e))


  def fireNodesInserted(e : TreeModelEvent) =
    listeners foreach (_.treeNodesInserted(e))

  def fireNodesRemoved(e : TreeModelEvent) =
    listeners foreach (_.treeNodesRemoved(e))

  def fireStructureChanged(e : TreeModelEvent) =
    listeners foreach (_.treeStructureChanged(e))


}


object TreeModelAdapter {

  def getChildren(graph : DependencyGraph, parent : NodeId) : List[DGNode] =
    (graph.content(parent) map graph.getNode).toList sortBy (_.name)

  def apply(graph:DependencyGraph): TreeModelAdapter = new GraphTreeModel(graph)

  def focused(graph:DependencyGraph, edge: DGEdge) : TreeModelAdapter = {
    val g = DependencyGraph.subGraph(graph, Set(edge.source, edge.target))
    new GraphTreeModel(g.addEdge(edge))
  }
  def subGraph(fullGraph : DependencyGraph, focus : Set[NodeId]): TreeModelAdapter ={
    new GraphTreeModel(DependencyGraph.subGraph(fullGraph,focus))
  }
}

class GraphTreeModel(val graph : DependencyGraph) extends TreeModelAdapter

