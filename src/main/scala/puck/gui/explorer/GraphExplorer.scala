package puck.gui.explorer

import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.JTree
import javax.swing.tree.TreePath

import puck.graph.io.VisibilitySet
import puck.graph.{NodeKind, DGNode, DependencyGraph, NodeId}

import scala.swing.event.Event
import scala.swing.{Component, Publisher, ScrollPane}

/**
 * Created by lorilan on 09/05/14.
 */

case class PuckTreeNodeClicked(graph : DependencyGraph, node : NodeId) extends Event
case class AccessGraphModified(graph : DependencyGraph) extends Event
case class SetVisible(ks : Seq[NodeKind]) extends Event

class GraphExplorer
(val hiddens :VisibilitySet,
 width : Int,
 height : Int)
  extends ScrollPane with Publisher {

/*
  minimumSize = new Dimension(width, height)
  preferredSize = minimumSize
*/



  def addChildren(graph : DependencyGraph,
                  ptn: PuckTreeNode): Unit = {
    import puck.graph.ShowDG._
    val nodeList = graph.content(ptn.nodeId).map(graph.getConcreteNode).toList
    nodeList.sortBy(_.name) foreach {
      (n: DGNode) =>
        val child = new PuckTreeNode(n.id, hiddens,
          showDG(graph)(nodeNameTypCord).shows(n))
        ptn add child
        addChildren(graph, child)
    }
  }

  var root : PuckTreeNode = _
  var graph : DependencyGraph = _

  reactions += {
    case AccessGraphModified(g) =>
      graph = g
      root = new PuckTreeNode(graph.rootId, hiddens, "<>")
      addChildren(graph, root)


      val tree: JTree = new JTree(root)
      tree.setCellRenderer(new PuckTreeCellRenderer(tree.getCellRenderer))

      tree.addMouseListener( new MouseAdapter {

        override def mouseClicked(e : MouseEvent) : Unit =  {
          val path : TreePath = tree.getPathForLocation(e.getX, e.getY)

          if(path!= null){
            path.getLastPathComponent match {
              case node : PuckTreeNode =>
                publish(PuckTreeNodeClicked(graph, node.nodeId))
                node.toggleFilter()
                tree.repaint()
              case _ => ()
            }
          }
        }
      })
      contents = Component.wrap(tree)
      this.repaint()
    case SetVisible(ks) =>
      root.kindVisible(graph, ks)
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
