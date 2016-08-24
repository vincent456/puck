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
import puck.ignore
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import java.{util => jutil}

import org.piccolo2d.PNode
import org.piccolo2d.util.PBounds
import puck.graph.NodeId
import puck.piccolo.util.{DecoratorGroup, IdIconTextNode}

import scala.collection.JavaConversions._
/**
  * Created by Loïc Girault on 31/05/16.
  */
class DGExpandableNode
( val titlePnode : IdIconTextNode
) extends DecoratorGroup with DGPNode{

  def id : NodeId = titlePnode.id

  val padding : Double = 5d
  val body = new PNode {
    def side : Int = squareSide(getChildrenCount)
    override def layoutChildren() : Unit = {
      var xOffset = 0d
      var yOffset = 0d
      var refHeight = 0d

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
    //setPropertyChangeParentMask(getPropertyChangeParentMask | PNode.PROPERTY_CODE_FULL_BOUNDS)
  }

  super.addChild(titlePnode)
  super.addChild(body)

  titlePnode.setOffset(margin.toDouble, margin.toDouble)
  body.setOffset(margin+padding, titlePnode.getFullBounds.getHeight + margin * 2)

//  def addChilrenEventPropagator(propertyName : String,
//                                propertyCode : Int): Unit ={
//    this.addPropertyChangeListener(propertyName,
//      new PropertyChangeListener() {
//        def propertyChange(evt: PropertyChangeEvent): Unit = {
//          val it  = body.getChildrenIterator.asInstanceOf[jutil.ListIterator[DGExpandableNode]]
//          it.foreach ( _.firePropertyChange(propertyCode, propertyName, null, null) )
//        }
//      })
//  }
//  addChilrenEventPropagator(PNode.PROPERTY_FULL_BOUNDS, PNode.PROPERTY_CODE_FULL_BOUNDS)
  this.addPropertyChangeListener(PNode.PROPERTY_FULL_BOUNDS,
    new PropertyChangeListener() {
      def propertyChange(evt: PropertyChangeEvent): Unit = {
        val it  = body.getChildrenIterator.asInstanceOf[jutil.ListIterator[DGExpandableNode]]
        it.foreach {
          child =>
            child.firePropertyChange(PNode.PROPERTY_CODE_FULL_BOUNDS,
              PNode.PROPERTY_FULL_BOUNDS, null, null)
        }
      }
    })

  this.addPropertyChangeListener(PNode.PROPERTY_PARENT,
    new PropertyChangeListener() {
      def propertyChange(evt: PropertyChangeEvent): Unit = {
        val isVisible = evt.getNewValue != null
        println(isVisible +" "+ getVisible)
        DGExpandableNode.this.setVisible(isVisible)
      }
    })

  this.addPropertyChangeListener(PNode.PROPERTY_VISIBLE,
    new PropertyChangeListener() {
      def propertyChange(evt: PropertyChangeEvent): Unit = {
        val children  = body.getChildrenIterator.asInstanceOf[jutil.ListIterator[DGExpandableNode]]
        children foreach ( child => child.setVisible(!child.getVisible) )
      }
    })


  def addContent(child : DGPNode) : Unit =
    ignore(body addChild child.toPNode)


  def rmContent(child : DGPNode) : Unit =
    ignore(body removeChild child.toPNode)

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




