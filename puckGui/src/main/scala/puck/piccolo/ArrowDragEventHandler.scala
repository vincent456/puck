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

import org.piccolo2d.event.{PDragEventHandler, PInputEvent}
import puck.actions.RedirectAction
import puck.gui.{Log, PuckControl}
import puck.piccolo.util.IdIconTextNode

/**
  * Created by Loïc Girault on 21/06/16.
  */
class ArrowDragEventHandler
( control : PuckControl,
  canvas : DGCanvas
) extends PDragEventHandler {
  setMinDragStartDistance(1d)

  override def shouldStartDragInteraction(event : PInputEvent) : Boolean =
    super.shouldStartDragInteraction(event) &&
      event.getPickedNode.isInstanceOf[PUses]


  override def drag(event : PInputEvent): Unit = {
    val uses = event.getPickedNode.asInstanceOf[PUses]
    uses.removeAllChildren()

    uses.addArrow(uses.source.arrowGlobalBounds.getCenter2D,
      event.getPosition)
  }

  override def endDrag(event: PInputEvent) : Unit  = {
    val pos = event.getPosition
    val path = canvas.getCamera.pick(pos.getX, pos.getY, 1)
    val uses = event.getPickedNode.asInstanceOf[PUses]
//    println( path.getPickedNode())
//    println( path.nextPickedNode())
//    println()
    path.getPickedNode match {
      case n : IdIconTextNode if uses.target.id != n.id =>
        control.graph.isAbstraction(uses.target.id, n.id) match {
          case Some(abs) =>
            new RedirectAction(control.Bus, uses.toNodeIdP, abs)(control.graph, control.graphUtils).apply()
          case None =>
            import puck.graph.ShowDG._
            control.Bus publish Log("redirection unhandled " + (control.graph, n.id).shows + " is not an abstraction")
            uses.removeAllChildren()
            uses.addArrow()
        }

      case _ =>
        uses.removeAllChildren()
        uses.addArrow()
    }

     super.endDrag(event)
  }
}
