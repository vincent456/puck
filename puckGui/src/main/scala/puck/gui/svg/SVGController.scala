package puck.gui.svg

import javax.swing.JPopupMenu

import org.w3c.dom.Element
import org.w3c.dom.svg.SVGGElement
import puck.graph._
import puck.gui._
import puck.gui.menus.EdgeMenu

import scala.swing.Publisher



class SVGController
(val genControl : PuckControl,
 console : ConsoleWithSelection)
  extends Publisher {

  genControl listenTo this

  def graph : DependencyGraph = genControl.graph
  val graphUtils = genControl.graphUtils
  val printingOptionsControl = genControl.printingOptionsControl


  val edgeMenuBuilder : NodeIdP => JPopupMenu = {
    e =>
      new EdgeMenu(this, e, printingOptionsControl, blurrySelection = true,
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
