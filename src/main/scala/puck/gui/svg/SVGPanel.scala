package puck.gui.svg

import java.awt.BorderLayout
import java.awt.event.{InputEvent, KeyEvent}
import java.util.regex.{Matcher, Pattern}
import javax.swing.{KeyStroke, JPanel}

import org.apache.batik.dom.GenericText
import org.apache.batik.dom.events.NodeEventTarget
import org.apache.batik.swing.JSVGCanvas
import org.apache.batik.swing.gvt.{GVTTreeRendererEvent, GVTTreeRendererAdapter}
import org.apache.batik.swing.svg.AbstractJSVGComponent
import org.apache.batik.util.{SVGConstants, XMLConstants}
import org.w3c.dom.{Node, NodeList, Element}
import org.w3c.dom.events.{Event, MouseEvent, EventListener}
import org.w3c.dom.svg._
import puck.graph.{NodeId, DGEdge}


class PUCKSVGCanvas
(panel : SVGPanel) extends JSVGCanvas {
  setDocumentState(AbstractJSVGComponent.ALWAYS_DYNAMIC)

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
        new SVGPanelListener(PUCKSVGCanvas.this, panel), false, null)
    }
  })

  def getElementById (id: String): Element =  getSVGDocument getElementById id

  def modify (f : () => Unit ) : Unit =
    getUpdateManager.getUpdateRunnableQueue.invokeLater(new Runnable {
      def run(): Unit = {
        println("modifying")
        f()}
    })

}

class SVGPanelListener
( canvas : PUCKSVGCanvas,
  panel : SVGPanel)
  extends EventListener {

  def controller = panel.controller

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

  private def edgeKindFromGElement(gelt: SVGGElement): DGEdge.EKind = {
    val l: NodeList = gelt.getChildNodes
    var i: Int = 0
    while (i < l.getLength) {
      val n: Node = l.item(i)
      if (n.getNodeName == "path") {
        if (n.asInstanceOf[Element].hasAttribute("stroke-dasharray"))
          return DGEdge.IsaK
        else
          return DGEdge.UsesK
      }
      i += 1; i - 1

    }

    DGEdge.UsesK
  }

  private val arrowPattern  = Pattern.compile("\\d+:(\\d+).{2}\\d+:(\\d+)")

  private def edgeFromGElement(gelt: SVGGElement): Option[DGEdge] = {
    val t = gelt.getFirstChild.asInstanceOf[SVGTitleElement]
    val title = t.getFirstChild.asInstanceOf[GenericText]
    val k: DGEdge.EKind = edgeKindFromGElement(gelt)
    val m: Matcher = arrowPattern.matcher(title.getData)
    if (m.find) {
      val source: Int = m.group(1).toInt
      val target: Int = m.group(2).toInt
      Some(k(source, target))
    }
    else None
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
    if(controller.selectedNodes.nonEmpty){
      controller.selectedNodes foreach setNodeColor
      controller.resetSelectedNodes()
    }
  }
  private def conditionalEdgeReset(): Option[DGEdge] = {
    controller.selectedEdge match {
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
              canvas.modify { () =>
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

          canvas.modify { () =>
            if(controller.selectedNodes.nonEmpty)
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
          canvas.modify { () =>
            if(controller.selectedNodes.nonEmpty)
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
              val menu: NodeRightClickMenu = new NodeRightClickMenu(controller, nodeId)
              menu.show(panel, evt.getClientX, evt.getClientY)
          }
        case _: SVGPathElement
             | _: SVGPolygonElement =>
          val line: Element = evt.getTarget.asInstanceOf[Element]
          checkIfEdgeAndGetGElement(line) foreach {
            gedge =>
              edgeFromGElement(gedge) foreach {
                e =>
                  val menu: EdgeRightClickMenu = new EdgeRightClickMenu(controller, e)
                  menu.show(panel, evt.getClientX, evt.getClientY)
              }
          }
      }


  def handleEvent(evt: Event) : Unit = {
    val mevt: MouseEvent = evt.asInstanceOf[MouseEvent]
    mevt.getButton match {
      case RIGHTBUTTON => handleRightClick(mevt)
      case LEFTBUTTON => handleLeftClick(mevt)
    }
  }
}


class SVGPanel
( doc : SVGDocument ) extends JPanel {
  val canvas = new PUCKSVGCanvas(this)
  canvas.setSVGDocument(doc)
  setLayout(new BorderLayout())
  add("Center", canvas)

  var controller : SVGController = _

  def graph = controller.graph



}
