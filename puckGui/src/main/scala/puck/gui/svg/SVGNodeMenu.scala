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

package puck.gui
package svg

import puck.graph._

import puck.gui.menus.{ConcreteNodeMenu, VirtualNodeMenu}

import scala.swing.{Action, PopupMenu}

object SVGNodeMenu{

  def apply(controller : SVGController,
            nodeId : NodeId)(implicit treeIcons: NodeKindIcons) : PopupMenu =
    controller.graph.getNode(nodeId) match {
      case n : ConcreteNode => new SVGConcreteNodeMenu(controller, n, treeIcons)
      case n : VirtualNode => new VirtualNodeMenu(controller,
        controller.graph,
        controller.graphUtils, n)
    }
}

class SVGConcreteNodeMenu
(controller: SVGController,
 node : ConcreteNode,
 treeIcons: NodeKindIcons)
  extends ConcreteNodeMenu(controller.genControl.Bus,
    controller.graph,
    controller.genControl.constraints,
    controller.graphUtils,
    controller.selectedNodes,
    controller.selectedEdge,
    blurryEdgeSelection = true,
    node,
    controller.printingOptionsControl,
    treeIcons) {

  override def init() = {
    super.init()

    addShowOptions()
  }

  private def addShowOptions() : Unit = {

    contents += new Action("Hide") {
      def apply() : Unit =
      printingOptionsControl.hide(graph, node.id)
    }
    contents += new Action("Focus") {
      def apply() : Unit =
      printingOptionsControl.focusExpand(graph, node.id, focus = true, expand = false)
    }
    contents += new Action("Focus & Expand") {
      def apply() : Unit =
      printingOptionsControl.focusExpand(graph, node.id, focus = true, expand = true)
    }
    contents += new Action("Show code") {
      def apply() : Unit =
      controller publish PrintCode(node.id)
    }

    if (graph.content(node.id).nonEmpty) {
      contents += new Action("Collapse") {
        def apply() : Unit =
        printingOptionsControl.collapse(graph, node.id)
      }
      contents += new Action("Expand") {
        def apply() : Unit =
        printingOptionsControl.focusExpand(graph, node.id, focus = false, expand = true)
      }
      contents += new Action("Expand all") {
        def apply() : Unit =
        printingOptionsControl.expandAll(graph, node.id)
      };()
    }
  }
}



