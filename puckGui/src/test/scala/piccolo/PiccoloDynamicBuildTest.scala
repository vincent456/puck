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
import org.piccolo2d.nodes.{PPath, PText}
import org.piccolo2d.util.PBounds
import puck._
import puck.graph._
import puck.piccolo.{DGPNode, TitledSquareNode, TypeDeclShapedPNode}

/**
  * Created by Loïc Girault on 23/05/16.
  */
class PiccoloDynamicBuildTest(g : DependencyGraph, aCanvas : PCanvas)
  extends PFrame("PiccoloDynamicBuildTest", false, aCanvas) {

  import puck.graph.ShowDG._


  def this(g : DependencyGraph) = this(g, null)

  def getSide(numChild : Int ) : Int = {

    def aux(i : Int) : Int =
      if(i * i >= numChild) i
      else aux(i + 1)

    aux(1)
  }

  def squareNode(n : NodeId) : PNode with DGPNode  = {
    val numChildren = g.content(n).size
    new TitledSquareNode(s"${(g, n).shows}", getSide(numChildren)) with DGPNode {
      val id = n
      def contentSize : Int = body.getChildrenCount
      def clearContent() : Unit = body.removeAllChildren()
    }
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
    kbManager.addKeyEventDispatcher( new KeyEventDispatcher {
      def dispatchKeyEvent( e : KeyEvent) : Boolean = {
        if(e.getID == KeyEvent.KEY_PRESSED) {
          val zoomHint = 5
          val currentBounds = getCanvas.getCamera.getViewBounds
          e.getKeyCode match {
            case KeyEvent.VK_ADD =>
              val newBounds =
                new PBounds( currentBounds.getX + zoomHint,
                  currentBounds.getY + zoomHint,
                  currentBounds.getWidth - zoomHint * 2,
                  currentBounds.getHeight - zoomHint * 2)
              ignore(getCanvas.getCamera.animateViewToCenterBounds(newBounds, true, 200))
            case KeyEvent.VK_MINUS | VK_NUMPAD_MINUS =>
              val newBounds =
                new PBounds( currentBounds.getX - zoomHint,
                  currentBounds.getY - zoomHint,
                  currentBounds.getWidth + zoomHint * 2,
                  currentBounds.getHeight + zoomHint * 2)

              ignore(getCanvas.getCamera.animateViewToCenterBounds(newBounds, true, 200))
            case c =>
              println( Integer.toHexString(c) + KeyEvent.getKeyText(c) + " ingnored")
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
      override def mousePressed(event: PInputEvent) : Unit = {
        def aux(n0 : PNode) : Unit = n0 match {
          case _: PText => ()
          case dgn : DGPNode =>
            if(dgn.contentSize == 0) addContent(dgn)
            else dgn.clearContent()
            ignore(getCanvas.getCamera.animateViewToCenterBounds(n0.getGlobalBounds, true, 500))
          case _  : PRoot =>

            ignore(getCanvas.getCamera.animateViewToCenterBounds(n0.getGlobalBounds, true, 500))
          case n  if n.getParent != null => aux(n.getParent)
          case n => ()
        }

        aux(event.getPickedNode)
      }
    })
  }

  def addContent(n : DGPNode) : Unit = {
    val pn = n.asInstanceOf[PNode]
    g.content(n.id) map g.getNode map { child =>
      child.kind.kindType match {
        case TypeDecl => TypeDeclShapedPNode.createClass(g, child.id)
        case NameSpace => squareNode(child.id)

      }
    } foreach pn.addChild
  }
}
