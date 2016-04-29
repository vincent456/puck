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
package menus

import javax.swing.{JMenuItem, JPopupMenu}

import puck.actions._
import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.rules.Redirection
import puck.gui.PrintingOptionsControl
import puck.gui.explorer.DGTreeIcons
import puck.gui.svg.actions.{Log, AutoSolveAction}

import scala.swing.Publisher

/**
  * Created by Loïc Girault on 16/12/15.
  */

object NodeMenu{

  type Builder = (DependencyGraph, Option[ConstraintsMaps], NodeId, List[NodeId], Option[NodeIdP]) => JPopupMenu

  def apply(bus : Publisher,
            graphUtils : GraphUtils,
            printingOptionsControl : PrintingOptionsControl,
            graph : DependencyGraph,
            cm : Option[ConstraintsMaps],
            nodeId : NodeId,
            selectedNodes: List[NodeId],
            selectedEdge : Option[NodeIdP])
           (implicit treeIcons: DGTreeIcons): JPopupMenu =
    graph.getNode(nodeId) match {
      case n : ConcreteNode =>
        new ConcreteNodeMenu(bus, graph, cm, graphUtils,
          selectedNodes, selectedEdge, blurryEdgeSelection = false,
          n, printingOptionsControl)
      case n : VirtualNode =>
        new VirtualNodeMenu(bus, graph, graphUtils, n)
    }
}

class ConcreteNodeMenu
(bus: Publisher,
 implicit val graph : DependencyGraph,
 scm : Option[ConstraintsMaps],
 implicit val graphUtils : GraphUtils,
 val selectedNodes: List[NodeId],
 val selectedEdge : Option[NodeIdP],
 blurryEdgeSelection : Boolean,
 node : ConcreteNode,
 printingOptionsControl: PrintingOptionsControl)
(implicit treeIcons: DGTreeIcons)
  extends JPopupMenu {

  init()

  def init(): Unit = {

    this.add(new RenameNodeAction(bus, node))
    this.addSeparator()

    val item = new JMenuItem(s"Abstract ${node.name} as")
    item.setEnabled(false)
    this.add(item)

    abstractionChoices foreach this.add

    if (childChoices.nonEmpty) {
      this.addSeparator()
      childChoices.foreach(this.add)
    }

    if (node.kind.isWritable) {
      this.add(new CreateInitalizerAction(bus,
        graph.getConcreteNode(graph.hostTypeDecl(node.id))))
    }

    this.addSeparator()
    this.add(new SetMutabilityAction(bus, node, !node.mutable))
    this.add(new RemoveNodeAction(bus, node))

    selectedNodes match {
      case Nil => ()
      case List(nid) => addOtherNodeSelectedOption(nid)
      case nodes => addOtherNodesSelectedOption(nodes)
    }

    selectedEdge.toList flatMap {
      case (source, target) =>
        graph.nodePlusDefAndParams(source) map ((_, target))
    } foreach addEdgeSelectedOption


    scm foreach {
      cm =>
        if ((graph, cm).isWronglyContained(node.id)
          || (graph, cm).isWronglyUsed(node.id)) {
          ignore(this add new AutoSolveAction(bus, cm, node, printingOptionsControl))
        }
    }

  }

  def abstractionChoices : Seq[JMenuItem] =
    node.kind.abstractionChoices.map { case (k, p) =>
      new JMenuItem(new AbstractionAction(bus, node, p, k))
    }

  def childChoices : Seq[JMenuItem] = {
    val ks = graph.nodeKinds.filter(node.kind.canContain)
    ks map {k => new JMenuItem(new AddNodeAction(bus, node, k))}
  }

  private def addAddIsaOption(sub: ConcreteNode, sup: ConcreteNode): Unit = {
    ignore( this add new AddIsaAction(bus, sub, sup) )

  }


  private def addOtherNodesSelectedOption(ids: List[NodeId]): Unit = {
    val sContainer = graph.container(ids.head)
    val sameContainer = ids.tail forall (graph.container(_) == sContainer)
    val kt = graph.kindType(ids.head)
    val sameKind = ids.tail forall (graph.kindType(_) == kt)
    if (!sameContainer)
      bus publish Log("Move multiple only available for nodes with same container")
    else if (!sameKind)
      bus publish Log("Move multiple only available for nodes with same kind")
    else {
      val selected: ConcreteNode = graph.getConcreteNode(ids.head)
      if (graph.canContain(node, selected))
        ignore( this add new MoveAction(bus, node, ids) )
    }

  }

  private def addOtherNodeSelectedOption(id: NodeId): Unit = {
    val selected: ConcreteNode = graph.getConcreteNode(id)
    if (graph.canContain(node, selected)) {
      this.add(new MoveAction(bus, node, List(id)))
    }

    //    val m: MergeMatcher = controller.transfoRules.
    //        mergeMatcherInstances.syntaxicMergeMatcher(selected)
    //
    //    if (m.canBeMergedInto(node, graph))
    if (selected.kind.kindType == node.kind.kindType)
      this.add(new MergeAction(bus, selected, node))


    if (selected.id != node.id) {
      if (selected.kind.canBe(node.kind)) addAddIsaOption(selected, node)
      if (node.kind.canBe(selected.kind)) addAddIsaOption(node, selected)
    }
  }

  private def addEdgeSelectedOption(edge: NodeIdP): Unit = {
    val (source, target) = edge

    def addRedirectAction(uses: NodeIdP) =
      graph.abstractions(target).foreach {
        abs =>
          if (abs.nodes.contains(node.id))
            this.add(new RedirectAction(bus, uses, abs))
      }

    def addChangeInitUsesAction(ctorDef: NodeId) =
      (graph.getRole(node.id), graph.getRole(target)) match {
        case (Some(Factory(ctorId)), Some(Initializer(_)))
          if ctorId == source =>
          this.add(abstractAction("Call to initialization in factory") {
            _ =>
              val g = Redirection.redirectSourceOfInitUseInFactory(graph.mileStone,
                ctorId, ctorDef, target, node.id)

              bus.publish(PushGraph(g))
          })
        case _ => ()
      }

    if(graph.uses(source, target))
        addRedirectAction(edge)


    graph.definitionOf(source).foreach {
      userDef =>
        if(graph.uses(userDef, target)) {
            addRedirectAction((userDef, target))
            addChangeInitUsesAction(userDef)
        }
    }


  }
}

class VirtualNodeMenu
(controller: Publisher,
 graph : DependencyGraph,
 graphUtils : GraphUtils,
 node : VirtualNode
) extends JPopupMenu {

  node.potentialMatches foreach {
    id =>
      val consumer = graph.getConcreteNode(id)
      import graphUtils.{transformationRules => TR}
      this.addMenuItem(s"Concretize as $consumer") { _ =>
        printErrOrPushGraph(controller,"Concretize action failure") {
          TR.merge.mergeInto(graph.mileStone, node.id, consumer.id)
        }
      }
  }

}