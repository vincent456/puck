package puck.gui.explorer

import javax.swing.JTree

import puck.graph.{DGNode, NodeId, DependencyGraph}

import scala.swing.{Component, ScrollPane, BorderPanel, Label}
import scala.swing.BorderPanel.Position
/**
  * Created by lorilan on 01/02/16.
  */
class StaticDGTreePane
( graph : DependencyGraph,
  focus : Set[NodeId],
  header : Label)
(implicit treeIcons : DGTreeIcons)
  extends  BorderPanel {

  def this(graph : DependencyGraph,
      focus : Set[NodeId],
      title : String,
      sTooltipText : Option[String] = None)(
    implicit treeIcons : DGTreeIcons) =
    this(graph, focus, new Label(title) {
      sTooltipText foreach (this.tooltip = _)
    })

  add(header, Position.North)

  val tree =
    new JTree(TreeModelAdapter.subGraph(graph, focus)) with DGTree {

    override def convertNodeToText(n : DGNode) : String =
      n.name + (if (focus contains n.id) " *" else "")
    def icons : DGTreeIcons = treeIcons

  }

  def selecteNodes = tree.selectedNodes

  add(new ScrollPane (Component.wrap(tree)), Position.Center)

}
