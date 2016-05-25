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
object TypeDeclShapedPNode {
  val fontSize = 8
  val lineHeight : Float = 10
  val width : Float = 100


  def createClass(g : DependencyGraph, cid : NodeId) : TypeDeclShapedPNode = {
    import puck.graph.ShowDG._
    val cln = new TypeDeclShapedPNode(g.getNode(cid).name, cid)
    g.content(cid) map (n =>
      (g, g getNode n).shows(desambiguatedLocalName)) foreach  cln.addMember

    cln
  }
}


import TypeDeclShapedPNode._
class TypeDeclShapedPNode
 (val title : String ,
  val id : NodeId)
  extends PPath.Float(new Rectangle2D.Float(0f, 0f, width, lineHeight))
  with DGPNode {

  animateToColor(Color.YELLOW, 0)

  def addTitle() : Unit =
    addChild(new PText(title) {
      setFont(new Font("SansSerif", Font.PLAIN, fontSize))
    })

  addTitle()

  def contentSize = getChildrenCount - 1 // minus title
  def addMember(sig : String) : Unit ={
    setHeight(getHeight + lineHeight)
    addChild(new PText(sig) {
      setFont(new Font("SansSerif", Font.PLAIN, fontSize))
    })
  }
  def clearContent() : Unit = {
    removeAllChildren()
    setHeight(lineHeight)
    addTitle()
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
