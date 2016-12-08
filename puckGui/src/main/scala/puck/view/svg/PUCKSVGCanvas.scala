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

package puck.view.svg

import java.awt.event.{InputEvent, KeyEvent}
import java.util.regex.{Matcher, Pattern}
import javax.swing.KeyStroke

import org.apache.batik.dom.GenericText
import org.apache.batik.dom.events.NodeEventTarget
import org.apache.batik.swing.JSVGCanvas
import org.apache.batik.swing.gvt.{GVTTreeRendererAdapter, GVTTreeRendererEvent}
import org.apache.batik.swing.svg.JSVGComponent
import org.apache.batik.util.{SVGConstants, XMLConstants}
import org.w3c.dom.{Element, Node, NodeList}
import org.w3c.dom.events.{Event, EventListener, MouseEvent}
import org.w3c.dom.svg._
import puck.graph.{NodeId, NodeIdP}
import puck.view.NodeKindIcons
import puck.view.svg.actions.SwingService

import scala.swing.{Component, PopupMenu}

object PUCKSVGCanvas {

  def apply() : PUCKSVGCanvas = new PUCKSVGCanvas {
    val eventListener = new EventListener {
      override def handleEvent(evt: Event): Unit = ()
    }
  }

  def apply(controller : SVGController,
            swingService: SwingService,
            treeIcons: NodeKindIcons): PUCKSVGCanvas = new PUCKSVGCanvas {
    val eventListener = new SVGCanvasListener(Component wrap this, controller, swingService, treeIcons)
  }

}

abstract class PUCKSVGCanvas extends JSVGCanvas {

  val eventListener : EventListener

  setDocumentState(JSVGComponent.ALWAYS_DYNAMIC)

  userAgent = new BridgeUserAgent(){
    override def openLink(elt: SVGAElement) : Unit = ()
  }

  def root = getSVGDocument.getRootElement.asInstanceOf[NodeEventTarget]

  import KeyEvent.{VK_EQUALS, VK_ADD, VK_MINUS, VK_SUBTRACT}
  import InputEvent.{CTRL_DOWN_MASK, CTRL_MASK, SHIFT_DOWN_MASK}
  import JSVGCanvas.{ZOOM_IN_ACTION, ZOOM_OUT_ACTION}
  import KeyStroke.getKeyStroke

  val ctrl_add = getKeyStroke(VK_EQUALS, CTRL_DOWN_MASK | SHIFT_DOWN_MASK)
  getInputMap.put(ctrl_add, ZOOM_IN_ACTION)
  val ctrl_add_numpad = getKeyStroke (VK_ADD, CTRL_DOWN_MASK)
  getInputMap.put (ctrl_add_numpad, ZOOM_IN_ACTION)
  val ctrl_minus = getKeyStroke (VK_MINUS, CTRL_MASK)
  getInputMap.put (ctrl_minus, ZOOM_OUT_ACTION)
  val ctrl_minus_numpad = getKeyStroke (VK_SUBTRACT, CTRL_MASK)
  getInputMap.put (ctrl_minus_numpad, ZOOM_OUT_ACTION)

  addGVTTreeRendererListener (new GVTTreeRendererAdapter() {
    override def gvtRenderingCompleted (e: GVTTreeRendererEvent) : Unit = {
      root.addEventListenerNS(XMLConstants.XML_EVENTS_NAMESPACE_URI,
        SVGConstants.SVG_EVENT_CLICK,
        eventListener, false, null)
    }
  })

  def getElementById (id: String): Element =  getSVGDocument getElementById id



}

class SVGCanvasListener
( menuInvoker : Component,
  controller : SVGController,
  swingService: SwingService,
  treeIcons: NodeKindIcons)
  extends EventListener {

  import swingService._
  private def checkIfNodeAndGetId(txtElt: Element): Option[Int] = {
    if (txtElt.getParentNode.getNodeName == "a") {
      val a: SVGAElement = txtElt.getParentNode.asInstanceOf[SVGAElement]
      try Some(a.getHref.getBaseVal.toInt)
      catch {
        case e: NumberFormatException => None
      }
    } else None
  }

  private def checkIfEdgeAndGetGElement(path: Element): Option[SVGGElement] = {
    if (path.getParentNode.getNodeName == "g") {
      val gelt: SVGGElement = path.getParentNode.asInstanceOf[SVGGElement]
      if (gelt.getId.startsWith("edge")) Some(gelt)
      else None
    } else None
  }

  private val idPattern = Pattern.compile("(\\d+:\\d+)|(nodeCluster\\d+)")

  private def edgeFromGElement(gelt: SVGGElement): Option[NodeIdP] = {
    val t = gelt.getFirstChild.asInstanceOf[SVGTitleElement]
    val title = t.getFirstChild.asInstanceOf[GenericText]

    val m: Matcher = idPattern.matcher(title.getData)

    def nextId() : Option[Int] = {
      if(m.find()) {
        val strId = m.group()
        if(strId.startsWith("nodeCluster")){
          Some(strId.substring("nodeCluster".length).toInt)
        } else {
          Some(strId.substring(strId.indexOf(":") + 1).toInt)
        }
      } else None
    }
    val ssrc = nextId()
    val stgt = nextId()
    (ssrc, stgt) match {
      case (Some(source), Some(target)) => Some((source, target))
      case _ => None
    }
  }

  private def changeEdgeColor(edge: SVGGElement, color: String) : Unit = {
    val l: NodeList = edge.getChildNodes

    var i: Int = 0
    while (i < l.getLength) {
      val n: Node = l.item(i)
      if (n.getNodeName == "path") {
        n.asInstanceOf[Element].setAttribute("stroke", color)
      }
      if (n.getNodeName == "polygon") {
        n.asInstanceOf[Element].setAttribute("stroke", color)
        n.asInstanceOf[Element].setAttribute("fill", color)
      }
      i += 1
    }
  }

  val setNodeColor : ((NodeId, String, Element)) => Unit = {
    case ((_, color, domElt)) =>
      domElt.setAttribute("fill", color)
  }
  private def resetAllNodes() : Unit = {
    if(controller.selectedSVGNodes.nonEmpty){
      controller.selectedSVGNodes foreach setNodeColor
      controller.resetSelectedNodes()
    }
  }
  private def conditionalEdgeReset(): Option[NodeIdP] = {
    controller.selectedSVGEdge match {
      case Some((e, color, domElt)) =>
        changeEdgeColor(domElt, color)
        controller.resetEdgeSelected()
        Some(e)
      case None => None
    }
  }

  private val selectColor: String = "blue"
  private val LEFTBUTTON: Short = 0
  private val RIGHTBUTTON: Short = 2

  private def handleLeftClick(evt: MouseEvent) : Unit = {
      val shiftPressed : Boolean = evt.getShiftKey
      evt.getTarget match {
        case txtElt: SVGTextElement =>
          checkIfNodeAndGetId(txtElt).foreach {
            nodeClickedId =>
              swingInvokeLater { () =>
                conditionalEdgeReset()

                if(!shiftPressed) {
                  val removed = controller.keepOnlySelectedNode(nodeClickedId)
                  removed foreach setNodeColor
                }
                controller.getSelectedNode(nodeClickedId) match {
                  case Some((_, color, domElt)) =>
                    controller.removeSelectedNode(nodeClickedId)
                    domElt.setAttribute("fill", color)
                  case None =>
                    controller.addNodeToSelection(nodeClickedId, txtElt)
                    txtElt.setAttribute("fill", selectColor)
                }
              }
          }
        case _: SVGPathElement
             | _: SVGPolygonElement =>
          val line: Element = evt.getTarget.asInstanceOf[Element]

          swingInvokeLater { () =>
            if(controller.selectedSVGNodes.nonEmpty)
              controller.resetSelectedNodes()

            val sPrevEdge = conditionalEdgeReset()
            checkIfEdgeAndGetGElement(line) foreach {
              gedge =>
                val sEdge = edgeFromGElement(gedge)

                sEdge foreach {
                  e =>
                    if (sEdge != sPrevEdge) {
                      val color: String = line.getAttribute("stroke")
                      controller.setEdgeSelected(e, gedge, color)
                      changeEdgeColor(gedge, selectColor)

                    }
                }
            }
          }
        case _ =>
          println("reset both")
          swingInvokeLater { () =>
            if(controller.selectedSVGNodes.nonEmpty)
              controller.resetSelectedNodes()
            conditionalEdgeReset()
            ()
          }
      }
    }

  private def handleRightClick(evt: MouseEvent) : Unit =
     evt.getTarget match {
        case txtElt: SVGTextElement =>
          checkIfNodeAndGetId(txtElt) foreach {
            nodeId =>
              val menu: PopupMenu = SVGNodeMenu(controller, controller.graph, nodeId)(treeIcons)
              menu.show(menuInvoker, evt.getClientX, evt.getClientY)
          }
        case _: SVGPathElement
             | _: SVGPolygonElement =>
          val line: Element = evt.getTarget.asInstanceOf[Element]
          checkIfEdgeAndGetGElement(line) foreach {
            gedge =>
              edgeFromGElement(gedge) foreach {
                e =>
                controller.edgeMenuBuilder(e).show(menuInvoker, evt.getClientX, evt.getClientY)
              }
          }
      }


  def handleEvent(evt: Event) : Unit = {
    val mevt: MouseEvent = evt.asInstanceOf[MouseEvent]
    swingInvokeLater( () =>
    mevt.getButton match {
      case RIGHTBUTTON => handleRightClick(mevt)
      case LEFTBUTTON => handleLeftClick(mevt)
      case _ => Console.err.println("mouse button event unknown")
    })
  }
}
