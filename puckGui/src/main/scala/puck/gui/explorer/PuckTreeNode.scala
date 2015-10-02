package puck.gui.explorer

import javax.swing.tree.DefaultMutableTreeNode

import puck.graph.{NodeKind, DependencyGraph, NodeId}
import puck.graph.io.{Visibility, Visible, Hidden}

import scala.swing.CheckBox

object PuckTreeNode {
  def isSelected : Visibility => Boolean = {
    case Visible => true
    case Hidden => false
  }
}

class PuckTreeNode(val nodeId : NodeId,
                   val explorer : GraphExplorer,
                   name : String)
  extends DefaultMutableTreeNode(nodeId){

  val checkBox = new CheckBox()
  checkBox.selected = true

  override def toString = name

  explorer.setVisibility(nodeId, Visible)

  override def getChildAt(i : Int) : PuckTreeNode =
    super.getChildAt(i).asInstanceOf[PuckTreeNode]


  def setVisible(visibility : Visibility, propagate : Boolean): Unit = {


   if(explorer.visibility(nodeId) != visibility){

     explorer.setVisibility(nodeId, visibility)

     if(visibility == Visible && this.getParent.isInstanceOf[PuckTreeNode]){
       this.getParent.asInstanceOf[PuckTreeNode].setVisible(Visible, propagate =false)
     }

     checkBox.selected = PuckTreeNode.isSelected(visibility)

   }

   if(propagate){
     for( i <- 0 until this.getChildCount){
       this.getChildAt(i).setVisible(visibility, propagate =true)
     }
   }
 }


  def kindVisible(graph : DependencyGraph, ks : Seq[NodeKind]): Unit = {
    val visibility =
      if(ks.contains(graph.getConcreteNode(nodeId).kind)) Visible
      else Hidden

    setVisible(visibility, propagate = false)

    for(i <- 0 until this.getChildCount){
      this.getChildAt(i).kindVisible(graph, ks)
    }
  }

  def hideWithName(graph : DependencyGraph, name : Seq[String]) : Unit =
    name match {
      case Seq(n) =>
        if(graph.getConcreteNode(nodeId).name == n)
          setVisible(Hidden, propagate = true)
      case hd +: tl =>
        if(graph.getConcreteNode(nodeId).name == hd)
          List.range(0, this.getChildCount).foreach{
            i =>
              this.getChildAt(i).asInstanceOf[PuckTreeNode].hideWithName(graph, tl)
          }
      case _ => ()
    }

/* def packageOnlyVisible(graph : DependencyGraph): Unit =
   kindVisible(graph, Seq(Package))*/


 def toggleFilter() : Unit = {
   setVisible(explorer.visibility(nodeId).opposite, propagate = true)
 }

}
