package puck.gui.explorer

import javax.swing.tree.DefaultMutableTreeNode

import puck.graph.NodeId

import scala.swing.CheckBox

/**
 * Created by lorilan on 10/07/14.
 */
object PuckTreeNode {
  def selected : Visibility => Boolean = {
    case Visible => true
    case Hidden => false
  }
}
class PuckTreeNode(val nodeId : NodeId,
                   val hiddens : HiddenSetBuilder,
                   name : String)
  extends DefaultMutableTreeNode(nodeId){

  val checkBox = new CheckBox()
  checkBox.selected = true

  override def toString = name

  var isVisible = true


  def setVisible(visibility : Visibility, propagate : Boolean){


   if(hiddens.visibility(nodeId) != visibility){

     hiddens.setVisibility(nodeId, visibility)

     if(visibility == Visible && this.getParent.isInstanceOf[PuckTreeNode]){
       this.getParent.asInstanceOf[PuckTreeNode].setVisible(Visible, propagate =false)
     }

     //agNode.setVisible(this.isVisible);
     checkBox.selected = this.isVisible

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
