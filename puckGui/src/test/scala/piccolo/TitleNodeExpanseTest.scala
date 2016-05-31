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

import java.awt.Color

import org.piccolo2d.event.{PBasicInputEventHandler, PInputEvent}
import org.piccolo2d.{PCanvas, PLayer, PNode}
import org.piccolo2d.extras.PFrame
import org.piccolo2d.extras.event.PSelectionEventHandler
import org.piccolo2d.nodes.{PPath, PText}
import puck.graph.{DependencyGraph, NodeId}
import puck.gui.NodeKindIcons
import puck.piccolo.{DGPNode, DecoratorGroup, IconTextNode, Register, TitledExpansableNode, ViewCommands}

/**
  * Created by Loïc Girault on 31/05/16.
  */


class TitleNodeExpanseTest (g : DependencyGraph,
                            aCanvas : PCanvas,
                            nk : NodeKindIcons)
  extends PFrame("TitleNodeExpanseTest", false, aCanvas) {

  implicit val nodeKindIcons : NodeKindIcons = nk
  val register = new Register()
  val nodeLayer = getCanvas.getLayer
  val edgeLayer = new PLayer()

  def getNode(nid : NodeId) : TitledExpansableNode  = {
    val titleNode = IconTextNode(g, nid)(nk)
    val n = new TitledExpansableNode(nid, titleNode)
    register += (nid -> n)
    n
  }

  def this(g : DependencyGraph,
           nk : NodeKindIcons) = this(g, null, nk)

  override def initialize() : Unit = {
    val n = getNode(0)
    nodeLayer addChild n
    getCanvas.removeInputEventListener(getCanvas.getPanEventHandler)

    // Create a selection handler so we can see that the decorator actually
    // works
    import scala.collection.JavaConversions.seqAsJavaList
    val selectableParents = List(n)
    //selectableParents.add(vdg)

    val ps: PSelectionEventHandler = new PSelectionEventHandler(getCanvas.getLayer, selectableParents)
    getCanvas.addInputEventListener(ps)

    getCanvas.addInputEventListener(new PBasicInputEventHandler() {

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

          println("mouseClicked : "+ event.getPickedNode.getClass)
          aux(event.getPickedNode)
        }


    })

  }

  def addContent(n : DGPNode) : Unit = {

    val pn = n.asInstanceOf[PNode]
    val content = g content n.id

    content map getNode foreach {
      c =>
        pn addChild c
    }
  }

}
