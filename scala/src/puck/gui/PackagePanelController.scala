package puck.gui

import puck.graph.{AGNode, AccessGraph}
import javax.swing.JTree

/**
 * Created by lorilan on 09/05/14.
 */
class PackagePanelController(private [this] var ag : AccessGraph) {

  def addChildren(ptn: PuckTreeNode){
    ptn.getAGNode.content.foreach{
      (n: AGNode) =>
        val child = new PuckTreeNode(n)
        ptn add child
        addChildren(child)
    }
  }

  val root = new PuckTreeNode(ag.root)
  addChildren(root)


  val tree: JTree = new JTree(root)
  tree.setCellRenderer(new PuckTreeCellRenderer(tree.getCellRenderer))

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
