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

package puck.gui.explorer

import javax.swing.JTree

import puck.graph.{DGNode, DependencyGraph, NodeId}
import puck.gui.NodeKindIcons

import scala.swing.{BorderPanel, Component, Label, ScrollPane}
import scala.swing.BorderPanel.Position
/**
  * Created by Loïc Girault on 01/02/16.
  */
class StaticDGTreePane
( graph : DependencyGraph,
  focus : Set[NodeId],
  header : Label)
(implicit treeIcons : NodeKindIcons)
  extends  BorderPanel {

  def this(graph : DependencyGraph,
      focus : Set[NodeId],
      title : String,
      sTooltipText : Option[String] = None)(
    implicit treeIcons : NodeKindIcons) =
    this(graph, focus, new Label(title) {
      sTooltipText foreach (this.tooltip = _)
    })

  add(header, Position.North)

  val tree =
    new JTree(TreeModelAdapter.subGraph(graph, focus)) with DGTree {

    override def convertNodeToText(n : DGNode) : String =
      n.name + (if (focus contains n.id) " *" else "")
    def icons : NodeKindIcons = treeIcons

  }

  def selecteNodes = tree.selectedNodes

  add(new ScrollPane (Component.wrap(tree)), Position.Center)

}
