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

import puck.control._
import puck.graph.DependencyGraph
import puck.view.menus.ConcreteNodeMenu
import puck.view.{NodeKindIcons, actionToMenuItem}

import scala.swing.{Action, PopupMenu, Publisher}

/**
  * Created by Loïc Girault on 07/06/16.
  */
object PiccoloNodeMenu {
  type Builder = (BasicGraphCanvas, DGExpandableNode) => PopupMenu
  def apply(controller: PuckControl,
            graph : DependencyGraph,
            nodeKindIcons: NodeKindIcons) : Builder =
    (canvas: BasicGraphCanvas,  node: DGExpandableNode) =>
      new PiccoloNodeMenu(controller, graph, nodeKindIcons, canvas, node)

  def readOnly(b : Publisher, g : DependencyGraph) : Builder =
     (c: BasicGraphCanvas,  n: DGExpandableNode) =>
    new PopupMenu with PReadOnlyMenu {
      val canvas = c
      val node: DGExpandableNode = n
      val bus: Publisher = b
      val graph: DependencyGraph = g

      contents += new Action("Infos"){
        def apply() : Unit = bus publish NodeClicked(g getConcreteNode n.id)
      }
      addReadOnlyOptions()
    }



}

trait PReadOnlyMenu {
 this : PopupMenu =>

  val canvas : BasicGraphCanvas
  val node : DGExpandableNode
  val bus : Publisher
  val graph : DependencyGraph


  def addReadOnlyOptions() : Unit = {

    contents += new Action("Show incomming uses"){
      def apply() : Unit = canvas addIncommingUses node
    }

    contents += new Action("Show outgoing uses"){
      def apply() : Unit = canvas addOutgoingUses node
    }

    contents += new Action("Hide incomming uses"){
      def apply() : Unit = canvas addIncommingUses node
    }

    contents += new Action("Hide outgoing uses"){
      def apply() : Unit = canvas addOutgoingUses node
    }

    contents += new Action("Hide") {
      def apply() : Unit = {
        bus publish Hide(graph, node.id)
        canvas hide node
      }
    }

    contents += new Action("Focus") {
      def apply() : Unit = {
        bus publish FocusExpand(graph, node.id, focus = true, expand = false)
        canvas focus node
      }
    }

    contents += new Action("Focus & Expand") {
      def apply() : Unit = {
        bus publish FocusExpand(graph, node.id, focus = true, expand = true)
        canvas focus node
        canvas expand node
      }
    }


    //          contents += new Action("Print"){
    //            def apply() : Unit =
    //              canvas.getNode(graph.rootId).toPNode.print()
    //          })



    contents += new Action("Show code") {
      def apply() : Unit =
        bus publish PrintCode(node.id)
    }

    if (graph.content(node.id).nonEmpty) {
      contents += new Action("Collapse") {
        def apply() : Unit = {
          bus publish Collapse(graph, node.id)
          canvas collapse node
        }
      }
      contents += new Action("Expand") {
        def apply() : Unit = {
          bus publish FocusExpand(graph, node.id, focus = false, expand = true)
          canvas expand node
        }
      }
      contents += new Action("Expand all") {
        def apply() : Unit = {
          bus publish ExpandAll(graph, node.id)
          canvas expandAll node
        }
      };()
    }
  }
}

class PiccoloNodeMenu
(val controller : PuckControl,
 g : DependencyGraph,
 nodeKindIcons: NodeKindIcons,
 val canvas : BasicGraphCanvas,
 val node : DGExpandableNode/*,
 selectedNodes : List[NodeId],
 selectedEdge: Option[NodeIdP]*/)
  extends ConcreteNodeMenu(
    controller, g,
    List(),//selectedNodes,
    None,//selectedEdge,
    blurryEdgeSelection = false,
    controller.graph getConcreteNode node.id,
    nodeKindIcons
  ) with PReadOnlyMenu {

  val bus = controller.Bus

  override def init() = {
    super.init()
    addReadOnlyOptions()
  }

}

