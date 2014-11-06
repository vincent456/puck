package puck.gui

import java.awt.event.{MouseEvent, MouseAdapter}
import javax.swing.tree.TreePath

import javax.swing.JTree

import puck.graph.{AccessGraph, NodeKind, NodeId}

import scala.swing.{Component, Dimension, Publisher, ScrollPane}
import scala.swing.event.Event

/**
 * Created by lorilan on 09/05/14.
 */

case class PuckTreeNodeClicked(graph : AccessGraph, node : NodeId) extends Event
case class AccessGraphModified(graph : AccessGraph) extends Event

class GraphExplorer(width : Int, height : Int)
  extends ScrollPane with Publisher {

  minimumSize = new Dimension(width, height)
  preferredSize = minimumSize


  def addChildren(graph : AccessGraph,
                  ptn: PuckTreeNode){
    graph.getNode(ptn.agNode).content foreach {
      (nid: NodeId) =>
        val n = graph.getNode(nid)
        val child = new PuckTreeNode(nid, n.nameTypeString)
        ptn add child
        addChildren(graph, child)
    }
  }

  reactions += {
    case e : AccessGraphModified =>
      val graph = e.graph
      val root = new PuckTreeNode(graph.rootId, "<>")
      addChildren(graph, root)


      val tree: JTree = new JTree(root)
      tree.setCellRenderer(new PuckTreeCellRenderer(tree.getCellRenderer))

      tree.addMouseListener( new MouseAdapter {

        override def mouseClicked(e : MouseEvent) {
          val path : TreePath = tree.getPathForLocation(e.getX, e.getY)

          if(path!= null){
            path.getLastPathComponent match {
              case node : PuckTreeNode =>
                publish(PuckTreeNodeClicked(graph, node.agNode))
                //obj.asInstanceOf[PuckTreeNode].toggleFilter()
                tree.repaint()
              case _ => ()
            }
          }
        }
      })
      contents = Component.wrap(tree)
      this.repaint()

  }




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
