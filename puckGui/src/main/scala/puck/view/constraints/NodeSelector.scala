package puck.view.constraints

import javax.swing.JTree

import puck.graph.{DependencyGraph, NodeId}
import puck.view.NodeKindIcons
import puck.view.explorer.{DGTree, TreeModelAdapter}

import scala.swing.Dialog.{Message, Options, Result}
import scala.swing.{Component, Dialog, ScrollPane}

/**
  * Created by Loïc Girault on 12/15/16.
  */
object NodeSelector {
  def apply(g : DependencyGraph,
            nodeKindIcons : NodeKindIcons) : Option[NodeId] = {
    val ns = new NodeSelector(g, nodeKindIcons)
    Dialog.showConfirmation(message = ns.peer,
      optionType = Options.OkCancel,
      messageType = Message.Plain) match {
      case Result.Ok => ns.selectedNode
      case Result.Cancel => None
    }
  }
}
class NodeSelector
(g : DependencyGraph,
 nodeKindIcons : NodeKindIcons) extends ScrollPane {

  var selectedNode : Option[NodeId] = None

  val tree = new JTree(TreeModelAdapter(g)) with DGTree {
    def icons: NodeKindIcons = nodeKindIcons
    addNodeClickedAction {
      (_, n) =>
        if(getSelectionCount == 1)
          selectedNode = Some(n.id)
        else selectedNode = None
    }
  }

  contents = Component.wrap(tree)
}
