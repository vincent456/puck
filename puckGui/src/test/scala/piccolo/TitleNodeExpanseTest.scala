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

import org.piccolo2d.event.{PBasicInputEventHandler, PInputEvent}
import org.piccolo2d.{PCanvas, PLayer, PNode}
import org.piccolo2d.extras.PFrame
import puck.graph.{DependencyGraph, NodeId}
import puck.gui.NodeKindIcons
import puck.piccolo.{Arrow, IconTextNode, Register, TitledExpansableNode}

import scala.swing._

/**
  * Created by Loïc Girault on 31/05/16.
  */

object TitleNodeExpanseTest {
  def edge(source : TitledExpansableNode, target : TitledExpansableNode) : PNode = {
    val srcBoundCenter = source.titlePnode.getGlobalFullBounds.getCenter2D
    val tgtBoundCenter = target.titlePnode.getGlobalFullBounds.getCenter2D

    Arrow(srcBoundCenter, tgtBoundCenter)
  }
}
import TitleNodeExpanseTest.edge
class TitleNodeExpanseTest (g : DependencyGraph,
                            aCanvas : PCanvas,
                            nk : NodeKindIcons)
  extends PFrame("TitleNodeExpanseTest", false, aCanvas) {

  implicit val nodeKindIcons : NodeKindIcons = nk
  val register = new Register[TitledExpansableNode]()
  val nodeLayer = getCanvas.getLayer
  val edgeLayer = new PLayer()
  getCanvas.getCamera.addLayer(0, edgeLayer)

  def getNode(nid : NodeId) : TitledExpansableNode  = {
    val titleNode = IconTextNode(g, nid)(nk)
    val n = new TitledExpansableNode(nid, titleNode)
    n.titlePnode.addInputEventListener(new PBasicInputEventHandler() {

      override def mousePressed(event: PInputEvent) : Unit =
        if(event.isRightMouseButton){
          val pos = event.getCanvasPosition
          val menu = new PopupMenu(){
            contents += new MenuItem(new Action("uses"){
              def apply() : Unit = addUsedBy(n)
            })
          }
          Swing.onEDT(menu.show(Component.wrap(getCanvas), pos.getX.toInt, pos.getY.toInt))
        }

      override def mouseClicked(event : PInputEvent) : Unit =
        if(event.getClickCount == 2 ) {
          if(n.contentSize == 0) addContent(n)
          else n.clearContent()
        }


    })
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
    //    import scala.collection.JavaConversions.seqAsJavaList
    //    val selectableParents = List(n)
    //    val ps: PSelectionEventHandler = new PSelectionEventHandler(getCanvas.getLayer, selectableParents)
    //    getCanvas.addInputEventListener(ps)

  }

  def addUsedBy(n : TitledExpansableNode ): Unit =
    g.usedBy(n.id) map (register.firstVisible(_, g)) foreach {
      used =>
        Swing.onEDT{
          val e = edge(n, used)
          edgeLayer addChild e
          //e.repaint()
        }
    }

  def addContent(n : TitledExpansableNode) : Unit = {

    val pn = n.toPNode
    val content = g content n.id

    content map getNode foreach {
      c =>
        pn addChild c
    }
  }

}
