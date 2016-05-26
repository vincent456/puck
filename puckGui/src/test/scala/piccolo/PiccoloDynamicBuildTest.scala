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

import java.awt.event.KeyEvent
import java.awt.{KeyEventDispatcher, KeyboardFocusManager}

import org.piccolo2d.{PCanvas, PNode, PRoot}
import org.piccolo2d.event.{PBasicInputEventHandler, PInputEvent}
import org.piccolo2d.extras.PFrame
import org.piccolo2d.nodes.PText
import org.piccolo2d.util.PBounds
import puck._
import puck.graph._
import puck.piccolo.{DGPNode, IconTextNode, TitledSquareNode, TypeDeclShapedPNode}
import TitledSquareNode.getSide
import puck.gui.NodeKindIcons

import scala.swing._
/**
  * Created by Loïc Girault on 23/05/16.
  */
object PiccoloDynamicBuildTest {

  implicit class BoundsOp(val b: PBounds) extends AnyVal {
    def copy(x: Double = b.getX, y: Double = b.getY,
             width: Double = b.getWidth,
             height: Double = b.getHeight) = new PBounds(x, y, width, height)
  }

}



import PiccoloDynamicBuildTest._
class PiccoloDynamicBuildTest(g : DependencyGraph,
                              aCanvas : PCanvas,
                              nk : NodeKindIcons)
  extends PFrame("PiccoloDynamicBuildTest", false, aCanvas) {

  def this(g : DependencyGraph,
           nk : NodeKindIcons) = this(g, null, nk)

  def squareNode(n : NodeId) : PNode with DGPNode  = {
    val numChildren = g.content(n).size
    val titleNode = IconTextNode(g, n)(nk)
    new TitledSquareNode(titleNode, getSide(numChildren)) with DGPNode {
      val id = n
      def contentSize : Int = body.getChildrenCount
      def clearContent() : Unit = body.removeAllChildren()
    }
  }

  def focus(n0 : PNode) : Unit = n0 match {
    case _ : TitledSquareNode
         | _  : PRoot =>
      println("focus ...")
      ignore(getCanvas.getCamera.animateViewToCenterBounds(n0.getGlobalBounds, true, 500))
    case n  if n.getParent != null => focus(n.getParent)
    case n => ()
  }

  override def initialize() : Unit = {
    val root = squareNode(g.rootId)
    addContent(root)

    getCanvas.getLayer.addChild(root)
    getCanvas.removeInputEventListener(getCanvas.getPanEventHandler)
    setFocusable(true)
    setFocusTraversalKeysEnabled(false)

    val kbManager = KeyboardFocusManager.getCurrentKeyboardFocusManager
    val VK_NUMPAD_MINUS = 0x6D

    def zoom(zoomHint : Int) : Unit = {
      val currentBounds = getCanvas.getCamera.getViewBounds
      val x = currentBounds.getX + zoomHint
      val y = currentBounds.getY + zoomHint
      val w = currentBounds.getWidth - zoomHint * 2
      val h = currentBounds.getHeight - zoomHint * 2
      if(w > 0 && h >0)
        ignore(getCanvas.getCamera.
          animateViewToCenterBounds(new PBounds(x, y, w, h), true, 200))

    }
    def zoomIn() : Unit = zoom(50)
    def zoomOut() : Unit = zoom(-50)

    kbManager.addKeyEventDispatcher( new KeyEventDispatcher {
      def dispatchKeyEvent( e : KeyEvent) : Boolean = {
        if(e.getID == KeyEvent.KEY_PRESSED) {
          val moveHint = 50

          e.getKeyCode match {
            case KeyEvent.VK_UP | KeyEvent.VK_KP_UP =>
              val currentBounds = getCanvas.getCamera.getViewBounds
              val newBounds = currentBounds.copy(y = currentBounds.getY - moveHint)
              ignore(getCanvas.getCamera.animateViewToCenterBounds(newBounds, true, 200))
            case KeyEvent.VK_DOWN | KeyEvent.VK_KP_DOWN =>
              val currentBounds = getCanvas.getCamera.getViewBounds
              val newBounds = currentBounds.copy(y = currentBounds.getY + moveHint)
              ignore(getCanvas.getCamera.animateViewToCenterBounds(newBounds, true, 200))

            case KeyEvent.VK_LEFT | KeyEvent.VK_KP_LEFT =>
              val currentBounds = getCanvas.getCamera.getViewBounds
              val newBounds = currentBounds.copy(x = currentBounds.getX - moveHint)
              ignore(getCanvas.getCamera.animateViewToCenterBounds(newBounds, true, 200))

            case KeyEvent.VK_RIGHT | KeyEvent.VK_KP_RIGHT =>
              val currentBounds = getCanvas.getCamera.getViewBounds
              val newBounds = currentBounds.copy(x = currentBounds.getX + moveHint)
              ignore(getCanvas.getCamera.animateViewToCenterBounds(newBounds, true, 200))

            case KeyEvent.VK_ADD => zoomIn()
            case KeyEvent.VK_MINUS | VK_NUMPAD_MINUS => zoomOut()
            case c =>
              println( Integer.toHexString(c) + " - " + KeyEvent.getKeyText(c) + " ignored")
          }
        }
        false
      }
    })

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
        if (event.getWheelRotation > 0) zoomOut()
        if (event.getWheelRotation < 0) zoomIn()
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

  def addContentSpecificLayout(n : DGPNode) : Unit = {
    val pn = n.asInstanceOf[PNode]
    g.content(n.id) map g.getNode map { child =>
      child.kind.kindType match {
        case TypeDecl => TypeDeclShapedPNode.createClass(g, child.id)
        case NameSpace => squareNode(child.id)
        case _ => puck.error("!")

      }
    } foreach pn.addChild
  }
}
