package puck.gui

import java.awt.{Component, BorderLayout}
import javax.swing.{JPanel, JTree}
import javax.swing.tree.{TreePath, TreeCellRenderer}

import puck.graph.NodeKind


/**
 * Created by lorilan on 10/07/14.
 */
class PuckTreeCellRenderer(delegate : TreeCellRenderer)
  extends JPanel with TreeCellRenderer {

  setLayout(new BorderLayout())
  setOpaque(false)

  def getTreeCellRendererComponent(tree: JTree, value: AnyRef, selected: Boolean,
                                   expanded: Boolean, leaf: Boolean, row: Int,
                                   hasFocus: Boolean): Component = {
    val renderer: Component =
      delegate.getTreeCellRendererComponent(tree, value, selected,
        expanded, leaf, row, hasFocus)


    removeAll()

    val path: TreePath = tree.getPathForRow(row)
    if (path != null) {
      path.getLastPathComponent match {
        case node : PuckTreeNode => add(node.checkBox.peer, BorderLayout.WEST)
        case _ => ()
      }
      add(renderer, BorderLayout.CENTER)
    }
    this
  }

}
