package puck.gui.explorer

import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.JTree
import javax.swing.tree.TreePath

import puck.graph._
import puck.graph.io.{Visible, Visibility, VisibilitySet}
import puck.gui.{SetTopLevelVisible, SetVisibleFromKind, PuckTreeNodeClicked, DGUpdate}


import scala.swing.{Component, Publisher, ScrollPane}
import ShowDG._


import VisibilitySet._
class GraphExplorer
(width : Int,
 height : Int)
  extends ScrollPane with Publisher {

  private var hiddens0 : VisibilitySet.T = _


  def setVisibility(id : NodeId, v : Visibility) =
    hiddens0 = hiddens0.setVisibility(id,v)

  def visibility(id : NodeId) : Visibility = 
    hiddens0.visibility(id)

  def visibilitySet = hiddens0

  def addChildren(graph : DependencyGraph,
                  ptn: PuckTreeNode): Unit = {
    val nodeList = graph.content(ptn.nodeId).map(graph.getConcreteNode).toList
    nodeList.sortBy(_.name) foreach {
      (n: DGNode) =>
        val child = new PuckTreeNode(n.id, this,
          (graph, n).shows(nodeNameTypCord))
        ptn add child
        addChildren(graph, child)
    }
  }

  var root : PuckTreeNode = _
  var graph : DependencyGraph = _

  reactions += {
    case DGUpdate(g) =>
      graph = g
      hiddens0 = VisibilitySet.allVisible(graph)
      root = new PuckTreeNode(graph.rootId, this, "<>")
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

      root.hideWithName(graph, Seq("root", "@primitive"))
      root.hideWithName(graph, Seq("root", "java"))

      contents = Component.wrap(tree)
      this.repaint()
    case SetVisibleFromKind(ks) =>
      root.kindVisible(graph, ks)
      this.repaint()
    case SetTopLevelVisible =>
      hiddens0  = VisibilitySet.allHidden(graph)

      for(i <- 0 until root.getChildCount){
        root.getChildAt(i).setVisible(Visible, propagate = false)
      }
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
