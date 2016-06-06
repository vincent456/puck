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

package puck.piccolo.util

import java.awt.event.KeyEvent
import java.awt.{KeyEventDispatcher, KeyboardFocusManager}

import org.piccolo2d.extras.PFrame
import org.piccolo2d.util.PBounds
import puck._
import puck.piccolo.BoundsOp
/**
  * Created by Loïc Girault on 31/05/16.
  */
object ViewCommands {

  val kbManager = KeyboardFocusManager.getCurrentKeyboardFocusManager
  val VK_NUMPAD_MINUS = 0x6D

  def zoom(frame : PFrame, zoomHint : Int) : Unit = {
    val currentBounds = frame.getCanvas.getCamera.getViewBounds
    val x = currentBounds.getX + zoomHint
    val y = currentBounds.getY + zoomHint
    val w = currentBounds.getWidth - zoomHint * 2
    val h = currentBounds.getHeight - zoomHint * 2
    if(w > 0 && h >0)
      ignore(frame.getCanvas.getCamera.
        animateViewToCenterBounds(new PBounds(x, y, w, h), true, 200))

  }
  def zoomIn(frame : PFrame) : Unit = zoom(frame, 50)
  def zoomOut(frame : PFrame) : Unit = zoom(frame, -50)





  def addGlobalKeyEventDispatcher(frame : PFrame) : Unit = {

    kbManager.addKeyEventDispatcher( new KeyEventDispatcher {
      def dispatchKeyEvent( e : KeyEvent) : Boolean = {
        if(e.getID == KeyEvent.KEY_PRESSED) {
          val moveHint = 50

          e.getKeyCode match {
            case KeyEvent.VK_UP | KeyEvent.VK_KP_UP =>
              val currentBounds = frame.getCanvas.getCamera.getViewBounds
              val newBounds = currentBounds.copy(y = currentBounds.getY - moveHint)
              ignore(frame.getCanvas.getCamera.animateViewToCenterBounds(newBounds, true, 200))
            case KeyEvent.VK_DOWN | KeyEvent.VK_KP_DOWN =>
              val currentBounds = frame.getCanvas.getCamera.getViewBounds
              val newBounds = currentBounds.copy(y = currentBounds.getY + moveHint)
              ignore(frame.getCanvas.getCamera.animateViewToCenterBounds(newBounds, true, 200))

            case KeyEvent.VK_LEFT | KeyEvent.VK_KP_LEFT =>
              val currentBounds = frame.getCanvas.getCamera.getViewBounds
              val newBounds = currentBounds.copy(x = currentBounds.getX - moveHint)
              ignore(frame.getCanvas.getCamera.animateViewToCenterBounds(newBounds, true, 200))

            case KeyEvent.VK_RIGHT | KeyEvent.VK_KP_RIGHT =>
              val currentBounds = frame.getCanvas.getCamera.getViewBounds
              val newBounds = currentBounds.copy(x = currentBounds.getX + moveHint)
              ignore(frame.getCanvas.getCamera.animateViewToCenterBounds(newBounds, true, 200))

            case KeyEvent.VK_ADD => zoomIn(frame)
            case KeyEvent.VK_MINUS | VK_NUMPAD_MINUS => zoomOut(frame)
            case c =>
              println( Integer.toHexString(c) + " - " + KeyEvent.getKeyText(c) + " ignored")
          }
        }
        false
      }
    })
  }
}
