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
import puck.gui.menus.NodeMenu


import scala.swing._

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

class GraphExplorer
( bus : Publisher,
  treeIcons : DGTreeIcons,
  graphUtils : GraphUtils,
  printingOptionsControl: PrintingOptionsControl)
  extends BoxPanel(Orientation.Vertical) {
  contents += new Label("DG Explorer")
  val treeWrapper = new ScrollPane()
  contents +=  treeWrapper



  private var selectedNodes = List[NodeId]()

  def display(graph: DependencyGraph,
              filter : Option[Either[DGEdge, Set[NodeId]]] = None): Unit = {

    if (graph.virtualNodes.nonEmpty) {
      val vn = graph.virtualNodes.head
      Choose("Concretize node",
        "Select a concrete value for the virtual node :",
        vn.potentialMatches.toSeq map graph.getConcreteNode) match {
        case None => ()
        case Some(cn) =>
          import Recording._
          val r2 = graph.recording.subRecordFromLastMilestone.concretize(vn.id, cn.id)
          bus publish RewriteHistory(r2)
      }
    }
    else {

      val model: DGTreeModel = filter match {
        case None => new FullDGTreeModel(graph)
        case Some(Left(e)) => DGTreeModel.focused(graph, e)
        case Some(Right(s)) => DGTreeModel.subGraph(graph, s)
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
                    NodeMenu(bus, graph, graphUtils,
                      selectedNodes, None, node.id, printingOptionsControl)
                  menu.add(new AbstractAction("Node infos") {
                    def actionPerformed(e: ActionEvent): Unit =
                      bus publish NodeClicked(node)
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
                  bus publish NodeClicked(node)
              case _ => ()
            }
          }
        }
      })


      def show(tp : TreePath): String =
        tp.getLastPathComponent.asInstanceOf[DGNode].name


      tree.addTreeSelectionListener(new TreeSelectionListener {
        def valueChanged(e: TreeSelectionEvent): Unit = {
          selectedNodes =
            tree.getSelectionPaths map (_.getLastPathComponent.asInstanceOf[DGNode].id) toList

        }
      })
      treeWrapper.contents = Component.wrap(tree)
      this.repaint()
    }
  }

}


