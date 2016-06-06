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

package puck.gui.explorer

import puck.actions.Choose
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.Recording
import puck.gui._
import javax.swing.JTree
import javax.swing.event.TreeModelEvent
import javax.swing.tree.TreePath

import puck.graph.transformations._
import Recording.RecordingOps
import puck.graph._
import puck.graph.transformations.Transformation.ChangeSource
import puck.gui.{NodeClicked, Pushed}
import puck.gui.menus.NodeMenu

import scala.swing._

/**
  * Created by Loïc Girault on 29/01/16.
  */
class MutableTreeModel(var graph : DependencyGraph)
  extends TreeModelAdapter {


  def treepath(g : DependencyGraph, id : NodeId) : TreePath = {
    new TreePath((g.containerPath(id) map g.getConcreteNode).toArray[Object])
  }

  def treeModelEvent(g : DependencyGraph, id : NodeId) : Option[TreeModelEvent] =
    g.container(id) map {
      parentId =>
        val parentTreePath = treepath(g, parentId)
        val node = g.getConcreteNode(id)
        val idx = TreeModelAdapter.getChildren(g, parentId) indexOf node
        new TreeModelEvent(this, parentTreePath, Array(idx), Array[Object](node))
    }

  def parentTreeModelEvent(g : DependencyGraph, id : NodeId) : Option[TreeModelEvent] =
    g.container(id) map {
      parentId =>
        val parentTreePath = treepath(g, parentId)
        new TreeModelEvent(this, parentTreePath, null, null)
    }


  def applyRec(newGraph: DependencyGraph, oldGraph : DependencyGraph, subRec : Recording) : Unit = {
    var reenactor = oldGraph

    subRec.foreach  {
      case Transformation.Add(Edge(ContainsKind(_, cted))) =>
        graph = newGraph
        treeModelEvent(newGraph, cted) foreach fireNodesInserted

      case Transformation.Remove(CNode(cted)) =>
        graph = reenactor
        treeModelEvent(reenactor, cted.id) foreach fireNodesRemoved
        reenactor = reenactor.removeNode(cted.id)._2

      case t @ ChangeSource(Contains(oldc, tgt), newc) =>
        graph = reenactor
        treeModelEvent(reenactor, tgt) foreach fireNodesRemoved
        reenactor = t.redo(reenactor)
        graph = reenactor
        treeModelEvent(reenactor, tgt) foreach fireNodesInserted

      case Transformation(_, RenameOp(id, oldName, newName)) =>
        graph = newGraph
        parentTreeModelEvent(newGraph, id) foreach fireStructureChanged

      case _ =>
    }

    graph = newGraph
  }

  def pushEvent(newGraph: DependencyGraph, oldGraph : DependencyGraph) : Unit = {
//    println("MutableTreeModel.pushEvent")
//    println(s"oldGraph = $oldGraph")
//    println(s"graph = $graph")
    assert(oldGraph eq graph)
    val subRec : Recording = newGraph.recording.subRecordFromLastMilestone.reverse
    applyRec(newGraph, oldGraph, subRec)

  }

  def popEvent(newGraph: DependencyGraph, oldGraph : DependencyGraph) : Unit = {
    assert(oldGraph eq graph)
    val subRec : Recording = oldGraph.recording.subRecordFromLastMilestone map (_.reverse)

    applyRec(newGraph, oldGraph, subRec)
  }
}

class DynamicDGTree
(model0 : MutableTreeModel,
 bus : Publisher,
 menuBuilder : NodeMenu.Builder,
 val icons: NodeKindIcons,
 override val constraints : Option[ConstraintsMaps])
  extends JTree(model0) with DGTree with Reactor {
  self : JTree =>

  this listenTo bus

  def model = getModel.asInstanceOf[MutableTreeModel]

  reactions += {
    case Popped(poppedGraph, newHead) =>
      model.popEvent(newHead, poppedGraph)
      //setModel(new MutableTreeModel(newHead))

    case EmptiedButOne(graph) =>
      setModel(new MutableTreeModel(graph))

    case Pushed(pushedGraph, previousHead) =>

      if (pushedGraph.virtualNodes.isEmpty)
        model.pushEvent(pushedGraph, previousHead)
      else {
        val vn = pushedGraph.virtualNodes.head
        Choose("Concretize node",
          "Select a concrete value for the virtual node :",
          vn.potentialMatches.toSeq map pushedGraph.getConcreteNode) match {
          case None => ()
          case Some(cn) =>
            val r2 = pushedGraph.recording.subRecordFromLastMilestone.concretize(vn.id, cn.id)
            bus publish RewriteHistory(r2)
        }
      }
  }

  addNodeClickedAction {
    (e, node) =>
      if (isRightClick(e)) {
        val menu: PopupMenu = menuBuilder(graph, node.id, selectedNodes, None)
        Swing.onEDT(menu.show(Component wrap this, e.getX, e.getY))
      }
      else if(node.kind.kindType != NameSpace)
        bus publish NodeClicked(node)
  }

}
