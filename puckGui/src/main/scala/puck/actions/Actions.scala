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

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph._
import puck.gui.PushGraph

import scala.swing.Publisher

class AddIsaAction
(controller : Publisher,
 sub : ConcreteNode,
 sup : ConcreteNode)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
extends AbstractAction(s"Add ${sub.name} isa ${sup.name}") {

  import graphUtils.{transformationRules => TR}

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Make SuperType Action failure") {
      TR.makeSuperType(graph.mileStone, sub.id, sup.id)()
    }
}



class RemoveEdgeAction
(controller : Publisher,
 edge : DGEdge)
(implicit graph : DependencyGraph)
  extends AbstractAction(s"Delete $edge") {

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      edge.kind match {
        case Isa => LoggedSuccess(edge.deleteIn(graph.mileStone))
        case _ => LoggedError(s"cannot remove remove ${edge.kind} edge")
      }
    }
}

class RemoveNodeAction
(controller : Publisher,
 node : ConcreteNode)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
extends AbstractAction(s"Delete node and children") {

  import graphUtils.{transformationRules => TR}

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      TR.removeConcreteNode(graph.mileStone, node)
    }
}


class RenameNodeAction
(controller : Publisher,
 node : ConcreteNode )
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
  extends AbstractAction("Rename") {

  import graphUtils.{transformationRules => TR}

  override def actionPerformed(e: ActionEvent): Unit = {
    showInputDialog("New name:", node.name).foreach {
      newName =>
          val g = TR.rename(graph.mileStone, node.id, newName)
          controller.publish(PushGraph(g))
    }
  }

}

class CreateInitalizerAction
(controller : Publisher,
 node : ConcreteNode)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
  extends AbstractAction(s"Create initializer of $node") {

  import graphUtils.{transformationRules => TR}

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      LoggedSuccess(TR.intro.initializer(graph.mileStone, node.id)._2)
    }
}

class SetMutabilityAction
(controller : Publisher,
 node : ConcreteNode,
 mutable : Boolean)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
  extends AbstractAction(s"Set $node " + (if(mutable) "mutable" else "immutable")) {

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Mutability Action failure"){
      LoggedSuccess(graph.setMutability(node.id, mutable))
    }
}

