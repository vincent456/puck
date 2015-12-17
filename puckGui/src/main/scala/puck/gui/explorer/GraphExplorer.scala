package puck.gui
package explorer

import java.awt
import java.awt.{MouseInfo, Color}
import java.awt.event.{MouseEvent, MouseAdapter}
import javax.swing.{JPopupMenu, JTree}
import javax.swing.tree.{TreePath, DefaultTreeCellRenderer}

import puck.actions.GraphController
import puck.gui.{NodeMenu, NodeClicked}
import puck.{StackListener, GraphStack}
import puck.graph._


import scala.swing.{Swing, Component, ScrollPane}


class DGTree(val graph : DependencyGraph)
  extends JTree( new DGTreeModel(graph) ) {

  override def convertValueToText
  (value: AnyRef, selected: Boolean,
   expanded: Boolean, leaf: Boolean,
   row: Int, hasFocus: Boolean) : String =
    value match {
      case null => ""
      case node : DGNode  => node.name
      case _ => ""
    }

  this setCellRenderer DGNodeWithViolationTreeCellRenderer

}




object DGNodeWithViolationTreeCellRenderer
 extends DefaultTreeCellRenderer {

  def sourceOfViolation(graph : DependencyGraph, nodeId : NodeId) : Boolean =
    (graph usedBy nodeId) exists (used => graph isViolation ((nodeId, used)))

  def targetOfViolation(graph : DependencyGraph, nodeId : NodeId) : Boolean =
    (graph usersOf nodeId) exists (user => graph isViolation ((user, nodeId)))

  override def getTreeCellRendererComponent(tree: JTree, value: scala.Any, selected: Mutability,
                                            expanded: Mutability, leaf: Mutability, row: NodeId,
                                            hasFocus: Mutability): awt.Component = {
    val c = super.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus)
    tree match {
      case dgTree : DGTree =>
        val node = value.asInstanceOf[DGNode]
        if(sourceOfViolation(dgTree.graph, node.id) ||
            targetOfViolation(dgTree.graph, node.id))
          c.setForeground(Color.RED)
        c
      case _ => c
    }
  }
}

class GraphExplorer
( width : Int,
  height : Int)
  extends ScrollPane
  with StackListener {


  def update(controller: GraphStack): Unit = {
    val tree: JTree = new DGTree(controller.graph)

    tree.addMouseListener( new MouseAdapter {

      override def mouseClicked(e : MouseEvent) : Unit =  {
        val path : TreePath = tree.getPathForLocation(e.getX, e.getY)

        if(path!= null){
          path.getLastPathComponent match {
            case node : DGNode =>
              if(isRightClick(e)){
                val menu : JPopupMenu = NodeMenu(controller.asInstanceOf[GraphController], node.id)
                Swing.onEDT(menu.show(GraphExplorer.this.peer, e.getX, e.getY))
              }
              else GraphExplorer.this.publish(NodeClicked(node))
            case _ => ()
          }
        }
      }
    })
    contents = Component.wrap(tree)
    this.repaint()
  }

}
