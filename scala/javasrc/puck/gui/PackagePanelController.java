package puck.gui;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JComponent;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import puck.graph.AccessGraph;
import puck.graph.nodes.AGNode;

public class PackagePanelController {

	private AccessGraph ag;
	
	private JTree tree;
	private DefaultMutableTreeNode top;
	
	private void addChildren(PuckTreeNode ptn){
		
		for(AGNode agn: ptn.getAGNode().getContains()){
			PuckTreeNode child_ptn = new PuckTreeNode(agn);
			ptn.add(child_ptn);
			addChildren(child_ptn);
		}
	}
	
	private void initTree(String rootName){
		
		top = new DefaultMutableTreeNode(rootName);
		
	    PuckTreeNode packageTreeNode;

		//TODO check JTree.DynamicUtilTreeNode !!
		for(AGNode pnode: ag.getRootNodes()){
				packageTreeNode = new PuckTreeNode(pnode);
				top.add(packageTreeNode);
				addChildren(packageTreeNode);
			
		}
		
		tree = new JTree(top);
		
		tree.setCellRenderer(new PuckTreeCellRenderer(tree.getCellRenderer())); 
		
		tree.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				TreePath path = tree.getPathForLocation(e.getX(), e.getY()); 
				
				if(path!= null){
					Object object = path.getLastPathComponent();
					if(object != null && object instanceof PuckTreeNode){
						((PuckTreeNode)object).toggleFilter();
						tree.repaint();
					}
				}
			}
		});
	}
	
	public PackagePanelController(AccessGraph ag, String rootName){
		this.ag= ag;
		this.initTree(rootName);
	}
	
	public JComponent getTreePanel(){
		return tree;
	}
	
	void setVisibilityAll(boolean b){
		int cc = top.getChildCount();
		for(int i =0; i<cc; i++){
			((PuckTreeNode) top.getChildAt(i)).setVisible(b, true);
		}
		tree.repaint();
	}
	
	void setVisiblePackagesOnly(){
		int cc = top.getChildCount();
		for(int i =0; i<cc; i++){
			((PuckTreeNode) top.getChildAt(i)).packageVisible();
		}
		tree.repaint();
	}

}
