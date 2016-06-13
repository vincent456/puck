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

package puck.piccolo

import puck.gui.actionToMenuItem
import puck.gui.menus.ConcreteNodeMenu
import puck.gui.{NodeKindIcons, PuckControl}

import scala.swing.{Action, PopupMenu}

/**
  * Created by Loïc Girault on 07/06/16.
  */
object PiccoloNodeMenu {
  type Builder = (DGCanvas, DGExpandableNode) => PopupMenu
  def apply(controller: PuckControl,
            nodeKindIcons: NodeKindIcons) : Builder =
    (canvas: DGCanvas,  node: DGExpandableNode) =>
      new PiccoloNodeMenu(controller, nodeKindIcons, canvas, node)
}
class PiccoloNodeMenu
(controller : PuckControl,
 nodeKindIcons: NodeKindIcons,
 canvas : DGCanvas,
 node : DGExpandableNode/*,
 selectedNodes : List[NodeId],
 selectedEdge: Option[NodeIdP]*/)
  extends ConcreteNodeMenu(
    controller.Bus,
    controller.graph,
    controller.constraints,
    controller.graphUtils,
    List(),//selectedNodes,
    None,//selectedEdge,
    blurryEdgeSelection = false,
    controller.graph getConcreteNode node.id,
    controller.printingOptionsControl,
    nodeKindIcons
  ){
  override def init() = {
    super.init()
    addShowOptions()
  }

  private def addShowOptions() : Unit = {

    contents += new Action("Show incomming uses"){
      def apply() : Unit = canvas addIncommingUses node
    }

    contents += new Action("Show outgoing uses"){
      def apply() : Unit = canvas addOutgoingUses node
    }

    /*contents += new Action("Hide") {
      def apply() : Unit =
        printingOptionsControl.hide(graph, node.id)
        canvas hide node

    }
//          contents += new Action("Print"){
//            def apply() : Unit =
//              canvas.getNode(graph.rootId).toPNode.print()
//          })

//    contents += new Action("Focus") {
//      def apply() : Unit =
//        printingOptionsControl.focusExpand(graph, node.id, focus = true, expand = false)
//    }
//    contents += new Action("Focus & Expand") {
//      def apply() : Unit =
//        printingOptionsControl.focusExpand(graph, node.id, focus = true, expand = true)
//    }
    contents += new Action("Show code") {
      def apply() : Unit =
        controller publish PrintCode(node.id)
    }

    if (graph.content(node.id).nonEmpty) {
      contents += new Action("Collapse") {
        def apply() : Unit =
          printingOptionsControl.collapse(graph, node.id)
          canvas collapse node

      }
      contents += new Action("Expand") {
        def apply() : Unit =
          printingOptionsControl.focusExpand(graph, node.id, focus = false, expand = true)
          canvas expand node
      }
      contents += new Action("Expand all") {
        def apply() : Unit =
          printingOptionsControl.expandAll(graph, node.id)
          canvas expandAll node
      };()
    }*/
  }
}
