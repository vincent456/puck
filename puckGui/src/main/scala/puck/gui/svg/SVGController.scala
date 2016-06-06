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

package puck.gui.svg

import org.w3c.dom.Element
import org.w3c.dom.svg.SVGGElement
import puck.graph._
import puck.gui._
import puck.gui.menus.EdgeMenu

import scala.swing.{PopupMenu, Publisher}



class SVGController
(val genControl : PuckControl,
 console : ConsoleWithSelection)
  extends Publisher {

  genControl.Bus listenTo this

  def graph : DependencyGraph = genControl.graph
  val graphUtils = genControl.graphUtils
  val printingOptionsControl = genControl.printingOptionsControl


  val edgeMenuBuilder : NodeIdP => PopupMenu = {
    e =>
      new EdgeMenu(genControl.Bus, e, printingOptionsControl, blurrySelection = true,
        genControl.constraints,
        graph, graphUtils )
  }

  type Color = String

  def selectedNodes: List[NodeId] = selectedSVGNodes map (_._1)

  def selectedEdge : Option[NodeIdP] = selectedSVGEdge map (_._1)

  var selectedNodes0: List[(NodeId, Color, Element)] = List()
  def selectedSVGNodes: List[(NodeId, Color, Element)] = selectedNodes0

  def getSelectedNode(nodeId: NodeId) : Option[(NodeId, Color, Element)]=
    selectedSVGNodes.find( _._1 == nodeId)

  def isSelected(nodeId: NodeId) : Boolean =
    selectedSVGNodes.exists( _._1 == nodeId)

  def removeSelectedNode(nodeId: NodeId) : Unit =
    selectedNodes0 = selectedNodes0.filter(_._1 != nodeId)

  def keepOnlySelectedNode(nodeId: NodeId) : List[(NodeId, Color, Element)] = {
    val (keep, others) = selectedSVGNodes partition (_._1 == nodeId)
    selectedNodes0 = keep
    others
  }

  val defaultColor = "black"

  def addNodeToSelection(id: NodeId, elt: Element): Unit = {
    val color =
      if(elt.getAttribute("fill").nonEmpty)
        elt.getAttribute("fill")
      else defaultColor
    selectedNodes0 :+= ((id, color, elt))
    val nodes = selectedNodes0 map {
      case (nid, _, _) => graph.getNode(nid)
    }
    console.displaySelection(nodes.mkString(", ") )
  }

  def resetSelectedNodes(): Unit = {
    selectedNodes0 = List()
    console.displaySelection("")
  }

  var selectedEdge0 : Option[(NodeIdP, Color, SVGGElement)] = None
  def selectedSVGEdge : Option[(NodeIdP, Color, SVGGElement)] = selectedEdge0


  def setEdgeSelected(dgEdge: NodeIdP, elt : SVGGElement, c : Color) = {
    selectedEdge0 = Some((dgEdge, c, elt))
    import ShowDG._
    console.displaySelection((graph, dgEdge).shows)
  }

  def resetEdgeSelected(): Unit = {
    selectedEdge0 = None
    console.displaySelection("")
  }

}
