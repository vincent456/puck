/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck
package gui
package explorer

import java.awt.event.ActionEvent
import javax.swing.{AbstractAction, JPopupMenu, JTree}

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
( control : PuckControl,
  nodeKindIcons : NodeKindIcons)
  extends BoxPanel(Orientation.Vertical){
  contents += new Label("DG Explorer")
  val treeWrapper = new ScrollPane()

  this listenTo control.Bus

  import control.{Bus => bus, graphUtils, printingOptionsControl}

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
    NodeMenu(bus, graphUtils, printingOptionsControl, _, _, _, _, _)(nodeKindIcons)

  reactions += {
    case GraphUpdate(graph) =>
      dynamicTree =
        new DynamicDGTree(new MutableTreeModel(graph), bus, menuBuilder, nodeKindIcons, control.constraints)
      displayGraph(buttonVisible = false, Component.wrap(dynamicTree))

    case ConstraintsUpdate(graph, cm) =>
      dynamicTree =
        new DynamicDGTree(new MutableTreeModel(graph), bus, menuBuilder, nodeKindIcons, Some(cm))
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
        def icons : NodeKindIcons = nodeKindIcons
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


