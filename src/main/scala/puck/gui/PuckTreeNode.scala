package puck.gui

import javax.swing.tree.DefaultMutableTreeNode

import puck.graph.{NodeId, NodeKind}

import scala.swing.CheckBox

/**
 * Created by lorilan on 10/07/14.
 */
class PuckTreeNode(val agNode : NodeId, name : String)
  extends DefaultMutableTreeNode(agNode){

  val checkBox = new CheckBox()
  checkBox.selected = true

  override def toString = name

  //var isVisible = true

  /*
 void setVisible(boolean isVisible, boolean propagate){
   if(this.isVisible != isVisible){

     this.isVisible = isVisible;
     if(isVisible && this.getParent() instanceof PuckTreeNode){
       ((PuckTreeNode) this.getParent()).setVisible(true, false);
     }

     agNode.setVisible(this.isVisible);
     checkBox.setSelected(this.isVisible);

   }

   if(propagate){
     int cc = this.getChildCount();
     for(int i=0; i<cc; i++){
       ((PuckTreeNode) this.getChildAt(i)).setVisible(isVisible, true);
     }
   }
 }


 void packageVisible(){
   boolean visibility = (agNode.getType() == NodeType.Package);

   this.isVisible = visibility;
   checkBox.setSelected(visibility);
   agNode.setVisible(visibility);

   int cc = this.getChildCount();
   for(int i=0; i<cc; i++){
     ((PuckTreeNode) this.getChildAt(i)).packageVisible();
   }
 }

 void toggleFilter(){
   setVisible(!isVisible, true);
 } */

}
