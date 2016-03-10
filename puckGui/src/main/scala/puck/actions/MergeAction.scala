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

import puck.graph.{GraphUtils, DependencyGraph, ConcreteNode, DGNode}

import scala.swing.Publisher

case class MergeAction
(controller : Publisher,
 consumed : DGNode,
 consumer : ConcreteNode)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
  extends AbstractAction(s"Merge $consumed into this") {

  import graphUtils.{transformationRules => TR}

  override def actionPerformed(e: ActionEvent): Unit =
    printErrOrPushGraph(controller,"Merge action failure") {

      val sConsumerHost= graph.container(consumer.id)
      val tg =
        if(graph.container(consumed.id) != sConsumerHost) {
          val ma = new MoveAction(controller, graph.getConcreteNode(sConsumerHost.get), List(consumed.id))
          ma.doMove
        }
        else puck.graph.LoggedSuccess(graph.mileStone)

      tg flatMap (TR.merge.mergeInto(_, consumed.id, consumer.id))
    }

}
