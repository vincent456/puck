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
import puck.piccolo.{Arrow, DGExpandableNode, IconTextNode, PUses, Register}

import scala.swing._

/**
  * Created by Loïc Girault on 31/05/16.
  */

class DGExpandableNodeTest(g : DependencyGraph,
                           aCanvas : PCanvas,
                           nk : NodeKindIcons)
  extends PFrame("DGExpandableNodeTest", false, aCanvas) {

  implicit val nodeKindIcons : NodeKindIcons = nk
  val register = new Register[DGExpandableNode]()
  val nodeLayer = getCanvas.getLayer
  val edgeLayer = new PLayer()
  getCanvas.getCamera.addLayer(0, edgeLayer)

  def clickEventHandler(n : DGExpandableNode) : PBasicInputEventHandler =
    new PBasicInputEventHandler() {

      override def mousePressed(event: PInputEvent) : Unit =
        if(event.isRightMouseButton){
          val pos = event.getCanvasPosition
          val menu = new PopupMenu(){
            contents += new MenuItem(new Action("uses"){
              def apply() : Unit = addUses(n)
            })
          }
          Swing.onEDT(menu.show(Component.wrap(getCanvas), pos.getX.toInt, pos.getY.toInt))
        }

      override def mouseClicked(event : PInputEvent) : Unit =
        if(event.getClickCount == 2 ) {
          if(n.contentSize == 0) addContent(n)
          else {
            n.fullContent.foreach {
              c =>
                val en = c.asInstanceOf[DGExpandableNode]
                en.usedBy.foreach(_.delete())
                en.usesOf.foreach(_.delete())
            }
            n.clearContent()
          }
        }


    }

  def getNode(nid : NodeId) : DGExpandableNode  = register.getOrElse(nid, {
    val titleNode = IconTextNode(g, nid)(nk)
    val n = new DGExpandableNode(nid, titleNode)
    n.titlePnode.addInputEventListener(clickEventHandler(n))
    n.addPropertyChangeListener(PNode.PROPERTY_PARENT, register.parentPropertyListener)
    n
  })

  def this(g : DependencyGraph,
           nk : NodeKindIcons) = this(g, null, nk)

  override def initialize() : Unit = {
    val n = getNode(0)
    nodeLayer addChild n
    println(getCanvas.getZoomEventHandler!=null)
    getCanvas.removeInputEventListener(getCanvas.getPanEventHandler)
    getCanvas.removeInputEventListener(getCanvas.getZoomEventHandler)

    // Create a selection handler so we can see that the decorator actually
    // works
    //    import scala.collection.JavaConversions.seqAsJavaList
    //    val selectableParents = List(n)
    //    val ps: PSelectionEventHandler = new PSelectionEventHandler(getCanvas.getLayer, selectableParents)
    //    getCanvas.addInputEventListener(ps)

  }


  def addUses(n : DGExpandableNode ): Unit = {
    g.usedBy(n.id) map (register.firstVisible(_, g)) foreach {
      used =>
        import puck.graph.ShowDG._
        (g, (n.id, used.id)).println

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
    g.usersOf(n.id) map (register.firstVisible(_, g)) foreach {
      user=>
        import puck.graph.ShowDG._
        (g, (user.id, n.id)).println
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

  def addContent(n : DGExpandableNode) : Unit = {

    val pn = n.toPNode
    val content = g content n.id

    content map getNode foreach {
      c =>
        pn addContent c
    }
  }

}
