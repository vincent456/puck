package puck.piccolo

import java.awt.{Color, Font}
import java.awt.geom.Rectangle2D
import java.util

import org.piccolo2d.PNode
import org.piccolo2d.nodes.{PPath, PText}
import puck.graph.{DependencyGraph, NodeId}

/**
  * Created by lorilan on 5/22/16.
  */
object ClassLayoutPNode {
  val fontSize = 8
  val lineHeight = 10
  val width = 100


  def createClass(g : DependencyGraph, cid : NodeId) : ClassLayoutPNode = {
    import puck.graph.ShowDG._
    val cln = new ClassLayoutPNode(g.getNode(cid).name)
    g.content(cid) map (n =>
      (g, g getNode n).shows(desambiguatedLocalName)) foreach  cln.addMember

    cln
  }
}


import ClassLayoutPNode._
class ClassLayoutPNode
 (title : String) extends PPath.Float(new Rectangle2D.Float(0, 0, width, lineHeight)){

  animateToColor(Color.YELLOW, 0)

  addChild(new PText(title) {
    setFont(new Font("SansSerif", Font.PLAIN, fontSize))
  })

  def addMember(sig : String) : Unit ={
    setHeight(getHeight + lineHeight)
    addChild(new PText(sig) {
      setFont(new Font("SansSerif", Font.PLAIN, fontSize))
    })
  }

  override def layoutChildren() : Unit = {
    val xOffset = 5d
    var yOffset = 0d

    import scala.collection.JavaConversions._
    val it  = getChildrenIterator.asInstanceOf[util.ListIterator[PNode]]
    it.foreach {
       n =>
         n.offset(xOffset, yOffset)
        yOffset += lineHeight
    }
  }

}
