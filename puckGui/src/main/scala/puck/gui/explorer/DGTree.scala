package puck.gui.explorer

import java.awt
import java.awt.Color
import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.event.{TreeSelectionEvent, TreeSelectionListener}
import javax.swing.tree.{DefaultTreeCellRenderer, TreePath}
import javax.swing.{JTree, Icon}

import puck.graph._

/**
  * Created by lorilan on 29/01/16.
  */
trait DGTreeIcons {
  def iconOfKind(k: NodeKind ) : Icon
}
trait DGTree {
  self : JTree =>

  def graph : DependencyGraph = getModel.asInstanceOf[TreeModelAdapter].graph
  def icons : DGTreeIcons

  def convertNodeToText(n : DGNode) : String = n.name

  override def convertValueToText
  (value: AnyRef, selected: Boolean,
   expanded: Boolean, leaf: Boolean,
   row: Int, hasFocus: Boolean) : String =
    value match {
      case null => ""
      case node : DGNode  => convertNodeToText(node)
      case _ => ""
    }

  this setCellRenderer DGNodeWithViolationTreeCellRenderer


  def addNodeClickedAction(action: (MouseEvent, DGNode) => Unit): Unit = {
    addMouseListener( new MouseAdapter {
      override def mouseClicked(e : MouseEvent) : Unit =  {
        val path : TreePath = self.getPathForLocation(e.getX, e.getY)
        if(path!= null){
          path.getLastPathComponent match {
            case n : DGNode => action(e, n)
            case _ => ()
          }
        }

      }
    })
  }

  private [this] var selectedNodes0 : List[NodeId] = List[NodeId]()

  def selectedNodes : List[NodeId] = selectedNodes0

  addTreeSelectionListener(new TreeSelectionListener {
    def valueChanged(e: TreeSelectionEvent): Unit = {
      Option(getSelectionPaths).foreach { sp =>
        selectedNodes0 =
          sp map(_.getLastPathComponent.asInstanceOf[DGNode].id) toList
      }

    }
  })

}

object DGNodeWithViolationTreeCellRenderer
  extends DefaultTreeCellRenderer {

  def sourceOfViolation(graph : DependencyGraph, nodeId : NodeId) : Boolean = {

    val usedByDef =
      graph.kindType(nodeId) match {
        case TypeConstructor
             | InstanceValueDecl
             | StaticValueDecl =>
          graph.definitionOf(nodeId) map graph.usedByExcludingTypeUse getOrElse Set[NodeId]()
        case _ => Set[NodeId]()
      }
    (graph usedByExcludingTypeUse nodeId) ++ usedByDef exists (used => graph isViolation ((nodeId, used)))
  }


  def targetOfViolation(graph : DependencyGraph, nodeId : NodeId) : Boolean =
    (graph usersOfExcludingTypeUse nodeId) exists (user => graph isViolation ((user, nodeId)))

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

        setIcon(dgTree.icons.iconOfKind(node.kind))

        c
      case _ => c
    }
  }
}

