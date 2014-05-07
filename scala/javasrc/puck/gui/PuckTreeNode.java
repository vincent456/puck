package puck.gui;

import javax.swing.JCheckBox;
import javax.swing.tree.DefaultMutableTreeNode;

import puck.graph.nodes.AGNode;
import puck.graph.nodes.AGNode.NodeType;

class PuckTreeNode 	extends DefaultMutableTreeNode {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private AGNode agn;
	private JCheckBox checkBox;
	private boolean isVisible; 
	
	PuckTreeNode(AGNode node){
		super(node);
		this.agn=node;
		
		this.checkBox = new JCheckBox();
		this.checkBox.setSelected(true);
		isVisible = true;
	}
	
	public String toString(){
		return agn.desambiguateLocalName();
	}
	
	JCheckBox getCheckBox() {
		return checkBox;
	}

	void setVisible(boolean isVisible, boolean propagate){
		if(this.isVisible != isVisible){

			this.isVisible = isVisible;
			if(isVisible && this.getParent() instanceof PuckTreeNode){
				((PuckTreeNode) this.getParent()).setVisible(true, false);
			}

			agn.setVisible(this.isVisible);
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
		boolean visibility = (agn.getType() == NodeType.Package);
		
		this.isVisible = visibility;
		checkBox.setSelected(visibility);
		agn.setVisible(visibility);

		int cc = this.getChildCount();
		for(int i=0; i<cc; i++){
			((PuckTreeNode) this.getChildAt(i)).packageVisible();
		}
	}
	
	void toggleFilter(){
		setVisible(!isVisible, true);
	}
	
	public AGNode getAGNode(){
		return agn;
	}
}