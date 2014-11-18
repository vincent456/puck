package puck.gui.explorer

import javax.swing.tree.DefaultMutableTreeNode

import puck.graph.NodeId
import puck.graph.io.{Visible, Hidden, Visibility, VisibilitySet}

import scala.swing.CheckBox

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


  def setVisible(visibility : Visibility, propagate : Boolean){


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


 /*def packageVisible(){
   val visibility = (agNode.getType() == NodeType.Package);

   this.isVisible = visibility
   checkBox.selected = visibility
   //agNode.setVisible(visibility)

   for(i <- 0 until this.getChildCount){
     this.getChildAt(i).asInstanceOf[PuckTreeNode].packageVisible()
   }
 }*/

 def toggleFilter(){
   setVisible(hiddens.visibility(nodeId).opposite, propagate = true)
 }

}
