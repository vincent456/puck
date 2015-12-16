package puck
package gui.explorer

import javax.swing.event.TreeModelListener
import javax.swing.tree.{TreePath, TreeModel}

import puck.graph.{NodeId, error, DGNode, DependencyGraph}

/**
  * Created by lorilan on 16/12/15.
  */
class DGTreeModel(graph : DependencyGraph) extends TreeModel{

  def getRoot: AnyRef = graph.root

  private def getChildren(parent: scala.Any) : Set[NodeId] =
    parent match {
      case node : DGNode =>
        graph.content(node.id)
      case _ => error()
    }

  def getIndexOfChild(parent: scala.Any, child: scala.Any): Int =
    getChildren(parent).toList map graph.getNode indexOf child

  def getChildCount(parent: scala.Any): Int =
    getChildren(parent).size

  def getChild(parent: scala.Any, index: Int): DGNode =
    graph.getNode(getChildren(parent).toList(index))

  def isLeaf(node: scala.Any): Boolean =
    getChildCount(node) == 0

  def valueForPathChanged(path: TreePath, newValue: scala.Any): Unit = ()
  def addTreeModelListener(l: TreeModelListener): Unit = ()
  def removeTreeModelListener(l: TreeModelListener): Unit = ()







}
