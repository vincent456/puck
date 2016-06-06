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
import puck.gui.{GraphStackEvent, NodeKindIcons, Popped, PrintCode, PuckControl, PuckMainPanel, Pushed, TreeViewHandler, ViewHandler, actionToMenuItem}
import org.piccolo2d.event.{PBasicInputEventHandler, PDragEventHandler, PInputEvent}
import org.piccolo2d.{PCanvas, PLayer, PNode}
import org.piccolo2d.extras.swing.PScrollPane
import puck.GraphStack
import puck.graph._
import puck.graph.transformations.Transformation.ChangeSource
import puck.graph.transformations.{Edge, RenameOp, Transformation}
import puck.gui.menus.{ConcreteNodeMenu, NodeMenu}

import scala.swing._

/**
  * Created by Loïc Girault on 02/06/16.
  */
object PiccoloViewHandler extends ViewHandler {

  override def toString = "PiccoloView"
  def installView(mainPanel: PuckMainPanel,
                  nodeKindIcons: NodeKindIcons) : Publisher = {
    new TreeViewHandler(mainPanel,
      scala.swing.Component.wrap(new PiccoloGraphExplorer(mainPanel.control, nodeKindIcons)))

  }
}


class DGCanvas
(val graphStack: GraphStack,
 implicit val nodeKindIcons : NodeKindIcons,
 menuBuilder : PiccoloNodeMenu.Builder)
  extends PCanvas {


  addInputEventListener(new PDragEventHandler())

  import graphStack.graph

  val register = new Register()
  val nodeLayer = getLayer
  val edgeLayer = new PLayer()
  getCamera.addLayer(0, edgeLayer)

  def getNode(nid : NodeId) : DGPNode  = register.getOrElse(nid, {
    val titleNode = DGTitleNode(graph, nid)
    val n = new DGExpandableNode(nid, titleNode)
    n.titlePnode.addInputEventListener(clickEventHandler(n))
    n.addPropertyChangeListener(PNode.PROPERTY_PARENT, register.parentPropertyListener)
    n
  })



  def applyRec(newGraph: DependencyGraph, oldGraph : DependencyGraph, subRec : Recording) : Unit = {
    var reenactor = oldGraph

    subRec.foreach  {
      case Transformation.Add(Edge(ContainsKind(cter, cted))) =>

        register.get(cter) foreach ( _ addContent getNode(cted) )

      case Transformation.Remove(Contains(cter, cted)) =>

        register.get(cter) foreach ( _ rmContent getNode(cted) )

      case t @ ChangeSource(Contains(oldc, tgt), newc) =>

        val n = getNode(tgt)

        register.get(oldc) foreach ( _ rmContent n )
        register.get(oldc) foreach ( _ addContent n )

      case Transformation(_, RenameOp(id, _, _)) =>
      val n = getNode(id).asInstanceOf[DGExpandableNode]
        import puck.graph.ShowDG._
        val newNameWithSig = (newGraph, newGraph getNode id).shows(desambiguatedLocalName)
        n.titlePnode.text.setText(newNameWithSig)


      case _ =>
    }

//    graph = newGraph
  }

  import puck.graph.Recording.RecordingOps
  def pushEvent(newGraph: DependencyGraph, oldGraph : DependencyGraph) : Unit = {
    //    println("MutableTreeModel.pushEvent")
    //    println(s"oldGraph = $oldGraph")
    //    println(s"graph = $graph")

    //assert(oldGraph eq graph)
    val subRec : Recording = newGraph.recording.subRecordFromLastMilestone.reverse
    applyRec(newGraph, oldGraph, subRec)
  }
  def popEvent(newGraph: DependencyGraph, oldGraph : DependencyGraph) : Unit = {
    //assert(oldGraph eq graph)
    val subRec : Recording = oldGraph.recording.subRecordFromLastMilestone map (_.reverse)

    applyRec(newGraph, oldGraph, subRec)
  }

  nodeLayer addChild getNode(0).toPNode

  removeInputEventListener(getPanEventHandler)
  removeInputEventListener(getZoomEventHandler)

  def clickEventHandler(n : DGExpandableNode) : PBasicInputEventHandler =
    new PBasicInputEventHandler() {

      override def mousePressed(event: PInputEvent) : Unit =
        if(event.isRightMouseButton){
          val pos = event.getCanvasPosition
          val menu = menuBuilder(DGCanvas.this, n)

//          menu.add(new AbstractAction("Print"){
//            def actionPerformed(e: ActionEvent): Unit =
//              getNode(graph.rootId).toPNode.print()
//          })
          Swing.onEDT(menu.show(Component wrap DGCanvas.this,
            pos.getX.toInt, pos.getY.toInt))
        }

      override def mouseClicked(event : PInputEvent) : Unit =
        if(event.getClickCount == 2 ) {
          if(n.contentSize == 0) expand(n)
          else collapse(n)
        }


    }

  def hide(n : DGPNode) : Unit =
    n.toPNode.setParent(null)

  def expand(n : DGPNode) : Unit = addContent(n)
  def collapse(n : DGExpandableNode) : Unit = {
      val newValue = ! n.getChild(0).getVisible
      n.fullContent.foreach {
        c =>
          val en = c.asInstanceOf[DGExpandableNode]
          en.usedBy.foreach(_.delete())
          en.usesOf.foreach(_.delete())
      }
      n.clearContent()
    }

  def expandAll(n : DGPNode) : Unit =
    addContent(n) foreach expandAll

  def addContent(n : DGPNode) : Seq[DGPNode] = {

    val pn = n.toPNode
    val content = graph content n.id

    val children =  content.toSeq map getNode
    children foreach pn.addContent
    children
  }

  def addUses(n : DGExpandableNode ): Unit = {
    graph.usedBy(n.id) map (register.firstVisible(_, graph)) foreach {
      used =>
        import puck.graph.ShowDG._
        (graph, (n.id, used.id)).println

        Swing.onEDT {
          val e = new PUses(n, used)
          if(!(n.usedBy contains e)) {
            n.usedBy += e
            used.usesOf += e
            edgeLayer addChild e
            e.repaint()
          }
        }
    }
    graph.usersOf(n.id) map (register.firstVisible(_, graph)) foreach {
      user=>
        import puck.graph.ShowDG._
        (graph, (user.id, n.id)).println
        Swing.onEDT{
          val e = new PUses(user, n)
          if(!(n.usedBy contains e)) {
            n.usesOf += e
            user.usedBy += e
            edgeLayer addChild e
            e.repaint()
          }
        }
    }
  }
}

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

    contents += new Action("Show uses"){
      def apply() : Unit = canvas addUses node
    }

    /*contents += new Action("Hide") {
      def apply() : Unit =
        printingOptionsControl.hide(graph, node.id)
        canvas hide node

    }
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

class PiccoloGraphExplorer
(control : PuckControl,
 nodeKindIcons: NodeKindIcons
) extends PScrollPane(new PCanvas()) with Publisher{

  var canvas : DGCanvas = _

  this listenTo control.Bus

  reactions += {
    case Popped(poppedGraph, newHead) =>
      canvas.popEvent(newHead, poppedGraph)
    //setModel(new MutableTreeModel(newHead))
    case Pushed(pushedGraph, previousHead) =>
      canvas.pushEvent(pushedGraph, previousHead)
    case evt : GraphStackEvent =>
      canvas = new DGCanvas(control.graphStack, nodeKindIcons, PiccoloNodeMenu(control,nodeKindIcons))
      setViewportView(canvas)


  }
}

