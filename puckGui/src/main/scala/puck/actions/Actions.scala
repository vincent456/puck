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

package puck.actions

import scala.swing.Action
import scala.swing.Publisher
import puck.graph._
import puck.gui.PushGraph

class AddIsaAction
(bus : Publisher,
 sub : ConcreteNode,
 sup : ConcreteNode)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
extends Action(s"Add ${sub.name} isa ${sup.name}") {

  import graphUtils.{Rules => TR}

  def apply() : Unit =
    printErrOrPushGraph(bus, "Make SuperType Action failure") {
        TR.makeSuperType(graph.mileStone, sub.id, sup.id)()
    }
}

class RemoveEdgeAction
(controller : Publisher,
 edge : DGEdge)
(implicit graph : DependencyGraph)
  extends Action(s"Delete $edge") {

  def apply() : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      edge.kind match {
        case Isa => LoggedSuccess(edge.deleteIn(graph.mileStone))
        case _ => LoggedError(s"cannot remove remove ${edge.kind} edge")
      }
    }
}

class RemoveNodeAction
(controller : Publisher,
 graph : DependencyGraph,
 node : ConcreteNode,
 graphUtils: GraphUtils)
extends Action(s"Delete node and children") {

  import graphUtils.{Rules => TR}

  def apply() : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      TR.remove.concreteNode(graph.mileStone, node)
    }
}


class RenameNodeAction
(controller : Publisher,
 graph : DependencyGraph,
 node : ConcreteNode,
 graphUtils: GraphUtils)
  extends Action("Rename") {

  import graphUtils.{Rules => TR}

  def apply(): Unit = {
    showInputDialog("New name:", node.name).foreach {
      newName =>
          val g = TR.rename(graph.mileStone, node.id, newName)
          controller.publish(PushGraph(g))
    }
  }

}

class CreateInitalizerAction
(controller : Publisher,
graph : DependencyGraph,
node : ConcreteNode,
graphUtils: GraphUtils)
  extends Action(s"Create initializer of $node") {

  import graphUtils.{Rules => TR}

  def apply() : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      LoggedSuccess(TR.intro.initializer(graph.mileStone, node.id)._2)
    }
}

