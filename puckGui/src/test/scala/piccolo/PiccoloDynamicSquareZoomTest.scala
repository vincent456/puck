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

package piccolo

import org.piccolo2d.{PCanvas, PLayer, PNode, PRoot}
import org.piccolo2d.event.{PBasicInputEventHandler, PInputEvent}
import org.piccolo2d.extras.PFrame
import org.piccolo2d.nodes.{PPath,  PText}
import puck._
import puck.graph._
import puck.piccolo.{DGPNode, IconTextNode, Register, TitledGridSquareNode, ViewCommands}
import puck.gui.NodeKindIcons

import scala.swing._
/**
  * Created by Loïc Girault on 23/05/16.
  */
object PiccoloDynamicSquareZoomTest {

  def edge(source : DGPNode, target : DGPNode) : PPath = {
    val bound1 = source.toPNode.getFullBounds.getCenter2D
    val bound2 = target.toPNode.getFullBounds.getCenter2D

    val e = new PPath.Float()
    e.moveTo(bound1.getX, bound1.getY)
    e.lineTo(bound2.getX, bound2.getY)


    println(e.getAllNodes.size())
    println(e.getBounds)

    e
  }

}
import puck.piccolo.BoundsOp
import puck.piccolo.TitledGridSquareNode.getSide
class TitledGridSquareDGPNode
( val id : NodeId,
  titleNode : PNode,
  numChildren : Int
) extends TitledGridSquareNode(titleNode, getSide(numChildren)) with DGPNode {
  def contentSize : Int = body.getChildrenCount
  def clearContent() : Unit = body.removeAllChildren()
}

import PiccoloDynamicSquareZoomTest._
class PiccoloDynamicSquareZoomTest(g : DependencyGraph,
                                   aCanvas : PCanvas,
                                   nk : NodeKindIcons)
  extends PFrame("PiccoloDynamicSquareZoomTest", false, aCanvas) {

  frame =>

  def this(g : DependencyGraph,
           nk : NodeKindIcons) = this(g, null, nk)

  val register = new Register()

  //nodeLayer underneath edgeLayer
  val nodeLayer = getCanvas.getLayer
  val edgeLayer = new PLayer()
  getCanvas.getCamera.addLayer(1, edgeLayer)

  def squareNode(nid : NodeId) : TitledGridSquareDGPNode  = {
    val numChildren = g.content(nid).size
    val titleNode = IconTextNode(g, nid)(nk)
    val n = new TitledGridSquareDGPNode(nid, titleNode, numChildren)
    register += (nid -> n)
    n
  }


  def addUsedBy(n0 : PNode ): Unit = n0 match {
  //  case n : DGPNode => addUsedBy(n)
    case n : TitledGridSquareDGPNode =>
      println(getCanvas.getCamera.getLayerCount)

      g.usedBy(n.id) map (register.firstVisible(_, g)) foreach {
        used =>
          import puck.graph.ShowDG._
          println(s"add ${(g, (n.id, used.id)).shows}")
          Swing.onEDT(edgeLayer addChild edge(n, used))
      }

      println("TitledSquareDGPNode ...")
      if(! n.isInstanceOf[DGPNode]) println("dafuq ??")
    case n if n.getParent != null =>
      addUsedBy(n.getParent)
    case n =>
      println(n.getClass +  " add uses ignored")
      n.getClass.getInterfaces foreach (i => println(i.getName))

  }

  def focus(n0 : PNode) : Unit = n0 match {
    case _ : TitledGridSquareNode
         | _  : PRoot =>
      println("focus on " + n0.getClass)
      ignore(getCanvas.getCamera.animateViewToCenterBounds(n0.getGlobalBounds, true, 500))
    case n  if n.getParent != null =>
      println("focus on parent ...")
      focus(n.getParent)
    case n => println(n.getClass + " : no focus")
  }

  override def initialize() : Unit = {
    val root = squareNode(g.rootId)
    addContent(root)


    nodeLayer addChild root

    getCanvas.removeInputEventListener(getCanvas.getPanEventHandler)
    setFocusable(true)
    setFocusTraversalKeysEnabled(false)


    ViewCommands.addGlobalKeyEventDispatcher(this)


    getCanvas.addInputEventListener(new PBasicInputEventHandler() {
      override def keyTyped(e: PInputEvent): Unit = println("canvas key typed !")
      override def keyReleased(e: PInputEvent): Unit = println("canvas key released !")
      override def keyPressed(event: PInputEvent) : Unit = println("canvas key pressed !")
    })


    getCanvas.addInputEventListener(new PBasicInputEventHandler() {
      override def mousePressed(event: PInputEvent) : Unit =
        if(event.isRightMouseButton){
          val pos = event.getCanvasPosition
          val menu = new PopupMenu(){
            contents += new MenuItem(new Action("Focus"){
              def apply() : Unit = focus(event.getPickedNode)
            })
            contents += new MenuItem(new Action("uses"){
              def apply() : Unit = addUsedBy(event.getPickedNode)
            })
          }
          Swing.onEDT(menu.show(Component.wrap(getCanvas), pos.getX.toInt, pos.getY.toInt))
        }


      override def mouseClicked(event : PInputEvent) : Unit =
        if(event.getClickCount == 2 ) {
          def aux(n0 : PNode) : Unit = n0 match {
            case _: PText => ()
            case dgn : DGPNode =>
              if(dgn.contentSize == 0) addContent(dgn)
              else dgn.clearContent()
            case n  if n.getParent != null => aux(n.getParent)
            case n => ()
          }

          aux(event.getPickedNode)
        }

      override def mouseDragged( event : PInputEvent) : Unit = {
        val currentBounds = getCanvas.getCamera.getViewBounds
        val w = currentBounds.getWidth / 2.0
        val h = currentBounds.getHeight / 2.0
        val p = event.getPosition
        val newBounds = currentBounds.copy(x = p.getX - w, y = p.getY - h )
        ignore(getCanvas.getCamera.animateViewToCenterBounds(newBounds, true, 200))
      }

      override def mouseWheelRotated(event: PInputEvent) : Unit = {
        if (event.getWheelRotation > 0) ViewCommands.zoomOut(frame)
        if (event.getWheelRotation < 0) ViewCommands.zoomIn(frame)
      }
    })
  }

  def addContent(n : DGPNode) : Unit = {
    val pn = n.asInstanceOf[PNode]

    val content = g content n.id

    val size =
      if( content.isEmpty ) 1
      else 8d / (10d * getSide(content.size))

    content map squareNode foreach {
      c =>
        pn addChild c
        c scale size
    }
  }

  //  def addContentSpecificLayout(n : DGPNode) : Unit = {
  //    val pn = n.asInstanceOf[PNode]
  //    g.content(n.id) map g.getNode map { child =>
  //      child.kind.kindType match {
  //        case TypeDecl => TypeDeclShapedPNode.createClass(g, child.id)
  //        case NameSpace => squareNode(child.id)
  //        case _ => puck.error("!")
  //
  //      }
  //    } foreach pn.addChild
  //  }
}
