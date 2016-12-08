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

package puck.view.explorer

import java.awt
import java.awt.Color
import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.event.{TreeSelectionEvent, TreeSelectionListener}
import javax.swing.tree.{DefaultTreeCellRenderer, TreePath}
import javax.swing.JTree

import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.view.NodeKindIcons


trait DGTree {
  self : JTree =>

  def graph : DependencyGraph = getModel.asInstanceOf[TreeModelAdapter].graph
  def constraints : Option[ConstraintsMaps] = None
  def icons : NodeKindIcons

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

  this setCellRenderer DGNodeWithForbiddenDependencyTreeCellRenderer


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

object DGNodeWithForbiddenDependencyTreeCellRenderer
  extends DefaultTreeCellRenderer {

  def sourceOfViolation(graph : DependencyGraph, constraints: ConstraintsMaps, nodeId : NodeId) : Boolean = {

    val usedByDef =
      graph.kindType(nodeId) match {
        case TypeConstructor
             | InstanceValue
             | StableValue =>
          graph.definitionOf(nodeId) map graph.usedByExcludingTypeUse getOrElse Set[NodeId]()
        case _ => Set[NodeId]()
      }
    (graph usedByExcludingTypeUse nodeId) ++ usedByDef exists (used => constraints.isForbidden(graph, nodeId, used))
  }


  def targetOfViolation(graph : DependencyGraph, constraints: ConstraintsMaps, nodeId : NodeId) : Boolean =
    (graph usersOfExcludingTypeUse nodeId) exists (user => constraints.isForbidden(graph, user, nodeId))

  override def getTreeCellRendererComponent(tree: JTree, value: scala.Any, selected: Boolean,
                                            expanded: Boolean, leaf: Boolean, row: NodeId,
                                            hasFocus: Boolean): awt.Component = {
    val c = super.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus)
    tree match {
      case dgTree : DGTree =>
        val node = value.asInstanceOf[DGNode]
        if(dgTree.constraints.nonEmpty &&
          (sourceOfViolation(dgTree.graph, dgTree.constraints.get, node.id) ||
          targetOfViolation(dgTree.graph, dgTree.constraints.get, node.id)))
          c.setForeground(Color.RED)

        setIcon(dgTree.icons.iconOfKind(node.kind))

        c
      case _ => c
    }
  }
}

