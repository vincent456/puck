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

package puck.graph.constraints.search

import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.search.SearchControl

/**
  * Created by Loïc Girault on 10/05/16.
  */
trait CheckViolation {
  val constraints: ConstraintsMaps

  def isViolationTarget(g: DependencyGraph, nid: NodeId): Boolean =
    (g, constraints).isWronglyUsed(nid) || (g, constraints).isWronglyContained(nid)

}

trait TargetFinder
  extends CheckViolation {

  val violationsKindPriority: Seq[NodeKind]

  def findTargets(graph : DependencyGraph,
                  l : Seq[NodeKind] = violationsKindPriority.toStream) : Seq[ConcreteNode] =  l match {
    case topPriority +: tl =>
      val tgts = graph.concreteNodes.toStream filter { n =>
        n.kind == topPriority && ((graph, constraints).wrongUsers(n.id).nonEmpty ||
          (graph, constraints).isWronglyContained(n.id))
      }
      if(tgts.nonEmpty) tgts
      else findTargets(graph, tl)

    case Nil => graph.concreteNodes.toStream filter { n => (graph, constraints).wrongUsers(n.id).nonEmpty ||
      (graph, constraints).isWronglyContained(n.id) }
  }

}

trait TerminalStateWhenTargetedViolationRemoved[T] {
  self : SearchControl[DecoratedGraph[T]] =>

  val constraints: ConstraintsMaps
  val violationTarget : ConcreteNode

  override def isTerminalState(t : DecoratedGraph[T]) : Boolean =
    !(t.graph, constraints).isWronglyContained(violationTarget.id) &&
      !(t.graph, constraints).isWronglyUsed(violationTarget.id)
}

trait TerminalStateWhenNoViolations[T] {
  self : SearchControl[DecoratedGraph[T]] =>

  val constraints: ConstraintsMaps

  override def isTerminalState(t : DecoratedGraph[T]) : Boolean =
    (t.graph, constraints).violations.isEmpty
}

