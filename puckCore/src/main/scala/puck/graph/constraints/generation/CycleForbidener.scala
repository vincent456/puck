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

package puck.graph.constraints.generation

import puck.graph.constraints.{LiteralRangeSet, Scope, Constraint, ConstraintsMaps}
import puck.graph.{NodeIdP, NameSpace, NodeId, DependencyGraph}
import puck.util.GreedyCycleRemover.WDGHelper
import puck.util.{GreedyCycleRemover, WeightedDirectedGraph}

/**
  * Created by Loïc Girault on 10/12/15.
  */
object CycleForbidener {
  val isNameSpace : (DependencyGraph, NodeId) => Boolean = {
    _.getNode(_).kind.kindType == NameSpace
  }

  def edgeToConstraint(e : NodeIdP) : Constraint = e match {
    case (user, used) =>
      val emptySet = LiteralRangeSet.empty
      val interloper = LiteralRangeSet(Scope(user))
      val owner = LiteralRangeSet(Scope(used))
      new Constraint(owner, emptySet, interloper, emptySet)
  }


  def genConstraints(g : DependencyGraph) : ConstraintsMaps ={
    val wdg = WeightedDirectedGraph.fromDG(g, isNameSpace)
    val remover = new GreedyCycleRemover(WDGHelper)
    val edgesToRemove = remover.edgesToRemove(wdg)
    (edgesToRemove map edgeToConstraint).
        foldLeft(ConstraintsMaps())(_.addHideConstraint(_))

  }
}
