package puck.gui

import java.awt.event.{MouseEvent, MouseAdapter}
import javax.swing.tree.TreePath

import puck.graph.{NodeKind, AGNode, AccessGraph}
import javax.swing.JTree

import scala.swing.Publisher
import scala.swing.event.Event

/**
 * Created by lorilan on 09/05/14.
 */

case class PuckTreeNodeClicked[Kind <: NodeKind[Kind]](node : PuckTreeNode[Kind]) extends Event

class PackagePanelController[Kind <: NodeKind[Kind]](private [this] var ag : AccessGraph[Kind])
  extends Publisher {

  def addChildren(ptn: PuckTreeNode[Kind]){
    ptn.agNode.content.foreach{
      (n: AGNode[Kind]) =>
        val child = new PuckTreeNode(n)
        ptn add child
        addChildren(child)
    }
  }

  val root = new PuckTreeNode[Kind](ag.root)
  addChildren(root)


  val tree: JTree = new JTree(root)
  tree.setCellRenderer(new PuckTreeCellRenderer(tree.getCellRenderer))

  tree.addMouseListener( new MouseAdapter {

    override def mouseClicked(e : MouseEvent) {
      val path : TreePath = tree.getPathForLocation(e.getX, e.getY)

      if(path!= null){
        path.getLastPathComponent match {
          case node : PuckTreeNode[Kind] =>
            publish(PuckTreeNodeClicked(node))
            //obj.asInstanceOf[PuckTreeNode].toggleFilter()
            tree.repaint()
          case _ => ()
        }
      }
    }
  })

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
