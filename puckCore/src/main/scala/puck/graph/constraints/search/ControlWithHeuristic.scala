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

package puck.graph
package constraints.search


import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.TransformationRules
import puck.search._


class ControlWithHeuristic
(val rules: TransformationRules,
 val initialGraph: DependencyGraph,
 val constraints: ConstraintsMaps,
 val violationsKindPriority : Seq[NodeKind]
) extends SearchControl[(DependencyGraph, Option[(ConcreteNode, AutomataState)])]
  with ActionGenerator
  with TargetFinder {

  def initialState: (DependencyGraph, Option[(ConcreteNode, AutomataState)]) = (initialGraph, None)

  def nextStates(g : DependencyGraph,
                 violationTarget : ConcreteNode,
                 automataState: AutomataState) : Seq[LoggedTry[(DependencyGraph, Option[(ConcreteNode, AutomataState)])]] =
    if(!isViolationTarget(g, violationTarget.id)) Seq(LoggedSuccess((g, None)))
    else automataState match {
      case 0 =>
        val s =
          setState((epsilon(g) ++ hostIntroAction(violationTarget)(g), Some((violationTarget, 1)))) ++
            setState((hostAbsIntro(violationTarget)(g), Some((violationTarget, 3)))) ++
            setState((moveContainerAction(violationTarget)(g), Some((violationTarget, 4))))
        assert(s.nonEmpty)
        s

      case 1 => val s = (moveAction(violationTarget)(g), Some((violationTarget, 2)))
        assert(s.nonEmpty)
        s
      case 2 => val s = (epsilon(g) ++ absIntro(violationTarget)(g), Some((violationTarget, 3)))
        assert(s.nonEmpty)
        s
      case 3 => val s = (redirectTowardAbstractions(violationTarget)(g), Some((violationTarget, 4)))
        assert(s.nonEmpty)
        s
      case _ => Seq()
    }

  def nextStates(state : (DependencyGraph, Option[(ConcreteNode, AutomataState)])) : Seq[LoggedTry[(DependencyGraph, Option[(ConcreteNode, AutomataState)])]] = {
    state match {
      case (g, Some((violationTarget, automataState))) => nextStates(g, violationTarget, automataState)
      case (g, None) => findTargets(g) flatMap (nextStates(g, _, 0))
    }
  }

}



