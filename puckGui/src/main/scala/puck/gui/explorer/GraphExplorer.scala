package puck
package gui
package explorer

import java.awt.event.ActionEvent
import javax.swing.{AbstractAction, JPopupMenu, JTree}
import javax.swing.tree.TreePath

import puck.graph._
import puck.gui.menus.NodeMenu


import scala.swing._

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

  this listenTo bus

  var dynamicTree : DynamicDGTree = _

  val showFullGraphButton = new Button(new Action("Show full graph") {
    def apply() = {
      displayGraph(buttonVisible = false, Component.wrap(dynamicTree))

    }
  })

  showFullGraphButton.visible = false
  contents += showFullGraphButton
  contents += treeWrapper

  val menuBuilder : NodeMenu.Builder =
    NodeMenu(bus, graphUtils, printingOptionsControl, _, _, _, _)


  reactions += {
    case GraphUpdate(graph) =>
      dynamicTree =
        new DynamicDGTree(new MutableTreeModel(graph), bus, menuBuilder, treeIcons)
      displayGraph(buttonVisible = false, Component.wrap(dynamicTree))
    case GraphFocus(graph, edge) =>
      displayGraph(buttonVisible = true,  filteredTree(graph, Left(edge)))
    case VisibilityEvent(g, v) =>
      import puck.graph.io.VisibilitySet.VisibilitySetOps
      displayGraph(buttonVisible = true,  filteredTree(g, Right(v.visibleNodes(g))))
  }

  def displayGraph(buttonVisible : Boolean, tree : Component) : Unit = {
    showFullGraphButton.visible = buttonVisible
    treeWrapper.contents = tree
    revalidate()
  }



  def filteredTree(graph: DependencyGraph,
                         filter : Either[DGEdge, Set[NodeId]]): Component = {


      val model: TreeModelAdapter = filter match {
        case Left(e) => TreeModelAdapter.focused(graph, e)
        case Right(s) => TreeModelAdapter.subGraph(graph, s)
      }

    Component.wrap(new JTree(model) with DGTree {
        def icons : DGTreeIcons = treeIcons
        addNodeClickedAction {
          (e, node) =>
            if (isRightClick(e)) {
              val menu: JPopupMenu = new JPopupMenu(){
                add(new AbstractAction("Node infos") {
                  def actionPerformed(e: ActionEvent): Unit =
                    bus publish NodeClicked(node)
                })
              }

              Swing.onEDT(menu.show(this, e.getX, e.getY))
            }
            else if(node.kind.kindType != NameSpace)
              bus publish NodeClicked(node)
        }
        GraphExplorer.expandAll(this)
      })
  }
}


