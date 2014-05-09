package puck.gui;

import java.awt.BorderLayout;
import java.awt.Component;

import javax.swing.JPanel;
import javax.swing.JTree;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreePath;

import puck.gui.PuckTreeNode;

public class PuckTreeCellRenderer extends JPanel implements TreeCellRenderer { 

	static final  long serialVersionUID =0;

	private TreeCellRenderer delegate; 

	public PuckTreeCellRenderer(TreeCellRenderer delegate){ 
		this.delegate = delegate; 
	
		setLayout(new BorderLayout()); 
		setOpaque(false); 

	} 


	public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row, boolean hasFocus){ 
		Component renderer = delegate.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus); 
		removeAll();

		TreePath path = tree.getPathForRow(row);            

		if(path!=null){

			Object onode = path.getLastPathComponent();

			//System.out.println("path = "+tree.getPathForRow(row));

			if(onode instanceof PuckTreeNode){
				PuckTreeNode ptnode = (PuckTreeNode) onode;
				add(ptnode.getCheckBox(), BorderLayout.WEST);
			}
			add(renderer, BorderLayout.CENTER);
		}
		return this; 
	}


	public TreeCellRenderer getDelegate() {
		return delegate;
	}


	public void setDelegate(TreeCellRenderer delegate) {
		this.delegate = delegate;
	} 
}