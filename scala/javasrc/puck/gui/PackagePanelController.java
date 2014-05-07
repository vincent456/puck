package puck.gui;

import javax.swing.JComponent;
import javax.swing.JTree;
//import javax.swing.tree.TreePath;

import puck.graph.AccessGraph;
import puck.graph.AGNode;

public class PackagePanelController {

	private AccessGraph ag;
	
	private JTree tree;
	private PuckTreeNode root;

	private void addChildren(PuckTreeNode ptn){

		for(AGNode agn:
                scala.collection.JavaConversions.asJavaIterable(ptn.getAGNode().getContent())){
			PuckTreeNode child_ptn = new PuckTreeNode(agn);
			ptn.add(child_ptn);
			addChildren(child_ptn);
		}
	}
	
	private void initTree(){
		//TODO check JTree.DynamicUtilTreeNode !!
		root = new PuckTreeNode(ag.root());
		addChildren(root);
			

		tree = new JTree(root);
		
		tree.setCellRenderer(new PuckTreeCellRenderer(tree.getCellRenderer())); 
		
		/*tree.addMouseListener(new MouseAdapter() {
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
		});*/
	}
	
	public PackagePanelController(AccessGraph ag){
		this.ag= ag;
		this.initTree();
	}
	
	public JComponent getTreePanel(){
		return tree;
	}
	
	/*void setVisibilityAll(boolean b){
		int cc = root.getChildCount();
		for(int i =0; i<cc; i++){
			((PuckTreeNode) root.getChildAt(i)).setVisible(b, true);
		}
		tree.repaint();
	}
	
	void setVisiblePackagesOnly(){
		int cc = root.getChildCount();
		for(int i =0; i<cc; i++){
			((PuckTreeNode) root.getChildAt(i)).packageVisible();
		}
		tree.repaint();
	}*/

}
