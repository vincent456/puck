package puck.gui.explorer

import javax.swing.tree.DefaultMutableTreeNode

import puck.graph.{NodeKind, NodeId, DependencyGraph}
import puck.graph.io.{Visible, Hidden, Visibility, VisibilitySet}

import scala.swing.CheckBox
import puck.javaGraph.nodeKind.{Package, Class}

/**
 * Created by lorilan on 10/07/14.
 */
object PuckTreeNode {
  def isSelected : Visibility => Boolean = {
    case Visible => true
    case Hidden => false
  }
}
class PuckTreeNode(val nodeId : NodeId,
                   val hiddens : VisibilitySet,
                   name : String)
  extends DefaultMutableTreeNode(nodeId){

  val checkBox = new CheckBox()
  checkBox.selected = true

  override def toString = name

  hiddens.setVisibility(nodeId, Visible)


  def setVisible(visibility : Visibility, propagate : Boolean): Unit = {


   if(hiddens.visibility(nodeId) != visibility){

     hiddens.setVisibility(nodeId, visibility)

     if(visibility == Visible && this.getParent.isInstanceOf[PuckTreeNode]){
       this.getParent.asInstanceOf[PuckTreeNode].setVisible(Visible, propagate =false)
     }

     checkBox.selected = PuckTreeNode.isSelected(visibility)

   }

   if(propagate){
     for( i <- 0 until this.getChildCount){
       this.getChildAt(i).asInstanceOf[PuckTreeNode].setVisible(visibility, propagate =true)
     }
   }
 }


  def kindVisible(graph : DependencyGraph, ks : Seq[NodeKind]): Unit = {
    val visibility =
      if(ks.contains(graph.getConcreteNode(nodeId).kind)) Visible
      else Hidden

    setVisible(visibility, propagate = false)

    for(i <- 0 until this.getChildCount){
      this.getChildAt(i).asInstanceOf[PuckTreeNode].kindVisible(graph, ks)
    }
  }

/* def packageOnlyVisible(graph : DependencyGraph): Unit =
   kindVisible(graph, Seq(Package))*/


 def toggleFilter() : Unit = {
   setVisible(hiddens.visibility(nodeId).opposite, propagate = true)
 }

}
