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

import java.{util => jutil}

import org.piccolo2d.{PLayer, PNode, PRoot}
import org.piccolo2d.event.{PDragEventHandler, PInputEvent}
import puck.control.PuckControl
import puck.control.actions.MoveAction
import puck.piccolo.util.IdIconTextNode


/**
  * Created by Loïc Girault on 07/06/16.
  */
class MoveNodeDragEventHandler
( control : PuckControl,
  canvas : DGCanvas
) extends PDragEventHandler {

  val dragLayer = new PLayer()
  canvas.getCamera.addLayer(0, dragLayer)
//  def printBounds(n : PNode) : Unit = {
//    println("bounds "  + n.getBounds)
//    println("fullBounds "  + n.getFullBounds)
//    println("globalBounds "  + n.getGlobalBounds)
//    println("globalFullBounds "  + n.getGlobalFullBounds)
//  }

  setMinDragStartDistance(1d)
  var copy : IdIconTextNode = _
  override def startDrag (event: PInputEvent) : Unit  = {
    event.getPickedNode match {
      case original : IdIconTextNode =>
        copy = original.clone()

        val bds = original.getGlobalFullBounds

        dragLayer addChild copy
        copy.translate(bds.getX, bds.getY)

        event.getPath.pushNode(copy)
        event.getPath.pushTransform(null)
      case _ => ()
      }

    super.startDrag(event)

  }

  override def shouldStartDragInteraction(event : PInputEvent) : Boolean =
    super.shouldStartDragInteraction(event) &&
      event.getPickedNode.isInstanceOf[IdIconTextNode]


  def getExpandableParent(n : PNode) : Option[DGExpandableNode] = n match {
    case n : DGExpandableNode => Some(n)
    case null | _ : PRoot | _ : PLayer => None
    case _ => getExpandableParent(n.getParent)
  }

  override def endDrag(event: PInputEvent) : Unit  = {
    if(copy!= null && (event.getPath.getPickedNode eq copy)) {
      event.getPath.popTransform(null)
      event.getPath.popNode(null)
      val l : jutil.ArrayList[PNode] = new jutil.ArrayList[PNode]()
      canvas.nodeLayer.findIntersectingNodes(copy.getGlobalFullBounds, l)
      val it =l.iterator()
      var sp : Option[DGExpandableNode] = None
      while(it.hasNext && sp.isEmpty){
        sp = getExpandableParent(it.next())
      }

      sp.foreach{ n =>
        import control.graph
        val dgn = graph.getNode(n.id)
        val g = graph.newGraph(mutabilitySet = control.mutabilitySet)
        if(g.canContain(dgn, g.getConcreteNode(copy.id)))
          new MoveAction(control.Bus, dgn, List(copy.id))(g, control.graphUtils).apply()
        else
          control.logger write s"$dgn cannot contain ${copy.text.getText}"
      }

      dragLayer.removeAllChildren()
      copy = null
    }

    super.endDrag(event)

  }
}
