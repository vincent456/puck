package puck
package gui.explorer

import javax.swing.event.TreeModelListener
import javax.swing.tree.{TreePath, TreeModel}

import puck.graph._

/**
  * Created by lorilan on 16/12/15.
  */

trait DGTreeModel extends TreeModel {
  val graph : DependencyGraph
}

class FullDGTreeModel(val graph : DependencyGraph) extends DGTreeModel{

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

class FocusedDGTreeModel(val graph : DependencyGraph, edge: DGEdge) extends DGTreeModel {

  def getRoot: AnyRef = graph.root

  def valueForPathChanged(path: TreePath, newValue: scala.Any): Unit = ()
  def addTreeModelListener(l: TreeModelListener): Unit = ()
  def removeTreeModelListener(l: TreeModelListener): Unit = ()

  val targetPath : Seq[NodeId] = graph.containerPath(edge.target)

  val srcPath : Seq[NodeId] =
    if(edge.kind == Contains) Seq()
    else graph.containerPath(edge.source)

  println(srcPath)
  println(targetPath)

  def getChildFromSeq(s : Seq[NodeId], parent :NodeId) : Option[NodeId] = {
    val idx = s.indexOf(parent)
    if(idx == -1) None
    else try Some(s(idx + 1))
    catch {
      case e : IndexOutOfBoundsException => None
    }
  }

  private def getChildren(parent: scala.Any) : Seq[NodeId] =
    parent match {
      case node : DGNode =>
        (getChildFromSeq(targetPath, node.id), getChildFromSeq(srcPath, node.id)) match {
          case (None, None) => Seq()
          case (None, Some(c)) => Seq(c)
          case (Some(c), None) => Seq(c)
          case (Some(c1), Some(c2)) => Seq(c1, c2)
        }
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


}


