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

import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import java.{util => jutil}

import org.piccolo2d.PNode
import org.piccolo2d.util.PBounds
import puck.graph.NodeId
import puck.piccolo.util.{DecoratorGroup, IconTextNode}

import scala.collection.mutable

/**
  * Created by Loïc Girault on 31/05/16.
  */
class DGExpandableNode
( val id: NodeId,
  val titlePnode : IconTextNode
) extends DecoratorGroup with DGPNode{

  val padding : Double = 5d
  val body = new PNode {
    def side : Int = squareSide(getChildrenCount)
    override def layoutChildren() : Unit = {
      var xOffset = 0d
      var yOffset = 0d
      var refHeight = 0d
      import scala.collection.JavaConversions._
      val it  = getChildrenIterator.asInstanceOf[jutil.ListIterator[PNode]]

      it.zipWithIndex.foreach {
        case (n, i) =>
          if(i % side == 0) {
            xOffset = 0
            if( i > 0 ) {
              yOffset += refHeight + padding
              refHeight = 0d
            }
          }
          if(n.getFullBounds.getHeight > refHeight)
            refHeight = n.getFullBounds.getHeight

          n.setOffset(xOffset, yOffset)
          xOffset += n.getFullBounds.getWidth + padding
      }
    }

  }

  val usedBy = mutable.ListBuffer[PUses]()
  val usesOf = mutable.ListBuffer[PUses]()

  super.addChild(titlePnode)
  super.addChild(body)

  titlePnode.setOffset(margin, margin)
  body.setOffset(margin+padding, titlePnode.getFullBounds.getHeight + margin * 2)


  def addContent(child : DGPNode) : Unit = {
    body addChild child.toPNode


//    child.toPNode.getClientProperties.
//      addAttribute(DGPNode.ATTRIBUTE_CONTAINER, this)

    this.addPropertyChangeListener(PNode.PROPERTY_FULL_BOUNDS,
      new PropertyChangeListener() {
        def propertyChange(evt: PropertyChangeEvent): Unit = {
          child.asInstanceOf[DGExpandableNode].
            firePropertyChange(PNode.PROPERTY_CODE_FULL_BOUNDS,
              PNode.PROPERTY_FULL_BOUNDS, null,
              child.toPNode.getFullBounds)
        }
      })

  }
  def rmContent(child : DGPNode) : Unit =
    body removeChild child.toPNode

  def content : Iterable[DGPNode] = {
    import scala.collection.JavaConversions._
    body.getChildrenReference.asInstanceOf[jutil.List[DGPNode]]
  }

  def contentSize: Int = body.getChildrenCount

  def clearContent(): Unit =  body.removeAllChildren()



  //def parent
  //global bounds used by edges as referential for source and target coordinates
  override def arrowGlobalBounds: PBounds = titlePnode.getGlobalFullBounds
}




