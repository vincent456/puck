package puck.gui
package explorer

import java.awt
import java.awt.Color
import java.awt.event.{ActionEvent, MouseEvent, MouseAdapter}
import javax.swing.event.{TreeSelectionEvent, TreeSelectionListener}
import javax.swing.{Icon, AbstractAction, JPopupMenu, JTree}
import javax.swing.tree.{TreePath, DefaultTreeCellRenderer}

import puck.actions.Choose
import puck.graph._
import puck.graph.io.PrintingOptions


import scala.swing.{Swing, Component, ScrollPane}

trait DGTreeIcons {
  def iconOfKind(k: NodeKind ) : Icon
}

trait DGTree {
  self : JTree =>

  def graph : DependencyGraph = getModel.asInstanceOf[DGTreeModel].graph
  def icons : DGTreeIcons

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

  def sourceOfViolation(graph : DependencyGraph, nodeId : NodeId) : Boolean = {

    val usedByDef =
      graph.kindType(nodeId) match {
        case TypeConstructor
          | InstanceValueDecl
          | StaticValueDecl =>
          graph.definitionOf(nodeId) map graph.usedBy getOrElse Set[NodeId]()
        case _ => Set[NodeId]()
      }
    (graph usedBy nodeId) ++ usedByDef exists (used => graph isViolation ((nodeId, used)))
  }


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

        setIcon(dgTree.icons.iconOfKind(node.kind))

        c
      case _ => c
    }
  }
}

object GraphExplorer {
  def expandAll(tree : JTree, startingIndex : Int=0, rowCount : Int = 0): Unit ={
    var i = startingIndex
    while(i< rowCount){
      tree expandRow i
      i += 1
    }
    if(tree.getRowCount != rowCount)
      expandAll(tree, rowCount, tree.getRowCount)
  }

}

class GraphExplorer(treeIcons : DGTreeIcons,
                    graphUtils : GraphUtils,
                    printingOptionsControl: PrintingOptionsControl)
  extends ScrollPane {

  reactions += {
    case GraphUpdate(graph) => display(graph, None)
    case GraphFocus(graph, edge) =>
      println("GraphExplorer receive GraphFocus")
      display(graph, Some(edge))
  }

  private var selectedNodes = List[NodeId]()

  def display(graph: DependencyGraph, filter : Option[DGEdge] = None): Unit = {

    if (graph.virtualNodes.nonEmpty) {
      val vn = graph.virtualNodes.head
      Choose("Concretize node",
        "Select a concrete value for the virtual node :",
        vn.potentialMatches.toSeq map graph.getConcreteNode) match {
       case None => ()
       case Some(cn) =>
         import ShowDG._
         val msg = s"Concretize ${(graph, vn).shows} into ${cn.name}"
        publish(PrintErrOrPushGraph(msg, LoggedSuccess(msg, graph.concretize(vn.id, cn.id))))
      }
    }
    else {

      val model: DGTreeModel = filter match {
        case None => new FullDGTreeModel(graph)
        case Some(e) => DGTreeModel.focused(graph, e)
          //new FocusedDGTreeModel(graph, e)
      }

      val tree: JTree = new JTree(model) with DGTree {
        def icons : DGTreeIcons = treeIcons
      }

      if(filter.nonEmpty)
        GraphExplorer.expandAll(tree)

      tree.addMouseListener(new MouseAdapter {

        override def mouseClicked(e: MouseEvent): Unit = {
          val path: TreePath = tree.getPathForLocation(e.getX, e.getY)

          if (path != null) {
            path.getLastPathComponent match {
              case node: DGNode =>
                if (isRightClick(e)) {
                  val menu: JPopupMenu =
                    NodeMenu(GraphExplorer.this, graph, graphUtils,
                      selectedNodes, None, node.id, printingOptionsControl)
                  menu.add(new AbstractAction("Node infos") {
                    def actionPerformed(e: ActionEvent): Unit =
                      GraphExplorer.this.publish(NodeClicked(node))
                  })
                  if (filter.nonEmpty)
                    menu.add(new AbstractAction("Show full graph") {
                      def actionPerformed(e: ActionEvent): Unit = {
                        GraphExplorer.this.display(graph, None)
                      }
                    })
                  Swing.onEDT(menu.show(tree, e.getX, e.getY))
                }
                else if(node.kind.kindType != NameSpace)
                  GraphExplorer.this.publish(NodeClicked(node))
              case _ => ()
            }
          }
        }
      })

      tree.addTreeSelectionListener(new TreeSelectionListener {
        def valueChanged(e: TreeSelectionEvent): Unit = {
          selectedNodes = e.getPaths.foldLeft(List[NodeId]()) {
            (l, tp) =>
              val n = tp.getLastPathComponent.asInstanceOf[DGNode]
              n.id :: l
          }
        }
      })
      contents = Component.wrap(tree)
      this.repaint()
    }
  }
}
