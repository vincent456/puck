/*
 * Copyright (c) 2008-2011, Piccolo2D project, http://piccolo2d.org
 * Copyright (c) 1998-2008, University of Maryland
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided
 * that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this list of conditions
 * and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright notice, this list of conditions
 * and the following disclaimer in the documentation and/or other materials provided with the
 * distribution.
 *
 * None of the name of the University of Maryland, the name of the Piccolo2D project, or the names of its
 * contributors may be used to endorse or promote products derived from this software without specific
 * prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package piccolo

import java.awt.geom.Rectangle2D
import java.util

import org.piccolo2d.PCanvas
import org.piccolo2d.PNode
import org.piccolo2d.event.PBasicInputEventHandler
import org.piccolo2d.event.PInputEvent
import org.piccolo2d.extras.PFrame
import org.piccolo2d.nodes.{PPath, PText}
import puck.graph.{DependencyGraph, NodeId}


class PiccoloTest(g : DependencyGraph, aCanvas : PCanvas)
  extends PFrame("HierarchyZoomExample", false, aCanvas) {

  import puck.graph.ShowDG._

  trait LayeredPNode {
    self : PNode =>

    val side : Int

    override def layoutChildren() : Unit = {
      var xOffset = 0d
      var yOffset = 0d

      import scala.collection.JavaConversions._
      val it  = getChildrenIterator.asInstanceOf[util.ListIterator[PNode]]
      it.zipWithIndex.foreach {
        case (n, i) =>
          if(i % side == 0) {
            xOffset = 0
            if( i > 0 )
              yOffset += (n.getHeight * n.getScale)
          }

          n.offset(xOffset, yOffset)
          xOffset += (n.getWidth * n.getScale)
      }
    }

//    override def paint(aPaintContext : PPaintContext) : Unit = {
//      val node = getIntegerAttribute("nodeId", -1)
//      println(s"${(g, node).shows} : (x,y) = ($getX, $getY), (w,h) = ($getWidth, $getHeight), scale = $getScale, ")
//      self.paint(aPaintContext)
//
//    }
  }


  def this(g : DependencyGraph) = this(g, null)

  def getSide(numChild : Int ) : Int = {

    def aux(i : Int) : Int =
      if(i * i >= numChild) i
      else aux(i + 1)

    aux(1)
  }

  override def initialize() : Unit = {
    val root = createHierarchy(g.rootId)
    getCanvas.getLayer.addChild(root)
    getCanvas.removeInputEventListener(getCanvas.getPanEventHandler)
    getCanvas.addInputEventListener(new PBasicInputEventHandler() {
      override def mousePressed(event: PInputEvent) : Unit =
        event.getPickedNode match {
          case _ : PText => ()
          case n =>
            puck.ignore(getCanvas.getCamera.animateViewToCenterBounds(n.getGlobalBounds, true, 500))
        }
    })
  }

  def createHierarchy(node : NodeId) : PNode = {

    val numChildren = g.content(node).size

    val s = getSide(numChildren)

    val result =
      new PPath.Float(new Rectangle2D.Float(0, 0, 100, 100))
        with LayeredPNode {
          val side = s

        addAttribute("nodeId", node)
      }

//    val text = new PText(s"${(g, node).shows}")
//    result.addChild(text)

    val size =
      if( numChildren == 0 ) 1
      else 1d / s

    println(s"${(g, node).shows} : $numChildren children (square of edge $s) scaled of $size")
    g.content(node).foreach {
      c =>
        val child = createHierarchy(c)
        child.scale(size)
        result.addChild(child)
    }

    result
  }


  def createHierarchy0(node : NodeId) : PNode = {
    val result = PPath.createRectangle(0, 0, 100, 100)
    val text = new PText(s"${(g, node).shows}")
    result.addChild(text)

    g.content(node).foreach {
      c =>
        val child = createHierarchy0(c)
        child.scale(0.5)
        result.addChild(child)
        child.offset(25, 25)
    }

    result
  }

}