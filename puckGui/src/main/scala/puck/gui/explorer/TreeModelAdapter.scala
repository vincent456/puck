package puck
package gui.explorer

import javax.swing.event.{TreeModelEvent, TreeModelListener}
import javax.swing.tree.{TreePath, TreeModel}

import puck.graph._

import scala.collection.mutable.ArrayBuffer

/**
  * Created by lorilan on 16/12/15.
  */


trait TreeModelAdapter extends TreeModel {
  def graph : DependencyGraph
  def getRoot: AnyRef = graph.root

  private [this] val listeners = new ArrayBuffer[TreeModelListener]()

  private def getChildren(parent: scala.Any) : List[DGNode] =
    parent match {
      case node : DGNode => TreeModelAdapter.getChildren(graph, node.id)
      case _ => error(s"expected DGNode got ${parent.getClass}")
    }

  def getIndexOfChild(parent: scala.Any, child: scala.Any): Int =
    getChildren(parent) indexOf child

  def getChildCount(parent: scala.Any): Int =
    getChildren(parent).size

  def getChild(parent: scala.Any, index: Int): DGNode =
    getChildren(parent)(index)

  def isLeaf(node: scala.Any): Boolean =
    getChildCount(node) == 0

  def valueForPathChanged(path: TreePath, newValue: scala.Any): Unit = ()

  def addTreeModelListener(l: TreeModelListener): Unit =
    ignore(listeners += l)

  def removeTreeModelListener(l: TreeModelListener): Unit =
    ignore(listeners -= l)

  def fireNodesChanged(e : TreeModelEvent) =
   listeners foreach (_.treeNodesChanged(e))


  def fireNodesInserted(e : TreeModelEvent) =
    listeners foreach (_.treeNodesInserted(e))

  def fireNodesRemoved(e : TreeModelEvent) =
    listeners foreach (_.treeNodesRemoved(e))

  def fireStructureChanged(e : TreeModelEvent) =
    listeners foreach (_.treeStructureChanged(e))


}


object TreeModelAdapter {

  def getChildren(graph : DependencyGraph, parent : NodeId) : List[DGNode] =
    (graph.content(parent) map graph.getNode).toList sortBy (_.name)

  def apply(graph:DependencyGraph): TreeModelAdapter = new GraphTreeModel(graph)

  def focused(graph:DependencyGraph, edge: DGEdge) : TreeModelAdapter = {
    val g = DependencyGraph.subGraph(graph, Set(edge.source, edge.target))
    new GraphTreeModel(g.addEdge(edge))
  }
  def subGraph(fullGraph : DependencyGraph, focus : Set[NodeId]): TreeModelAdapter ={
    new GraphTreeModel(DependencyGraph.subGraph(fullGraph,focus))
  }
}

class GraphTreeModel(val graph : DependencyGraph) extends TreeModelAdapter

