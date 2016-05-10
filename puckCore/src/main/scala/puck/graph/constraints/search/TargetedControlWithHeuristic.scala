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
import puck.graph.transformations.TransformationRules
import puck.search.SearchControl

/**
  * Created by Loïc Girault on 10/05/16.
  */

trait Heuristic extends ActionGenerator {

  def nextStates
  (violationTarget : ConcreteNode )
  (g : DependencyGraph,
   automataState : AutomataState) : Seq[LoggedTry[DecoratedGraph[AutomataState]]] = {
    automataState match {
      case 0 =>
        val s =
          decorate(epsilon(g) ++ hostIntroAction(violationTarget)(g), 1) ++
            decorate(hostAbsIntro(violationTarget)(g), 3) ++
            decorate(moveContainerAction(violationTarget)(g), 4)
        assert(s.nonEmpty)
        s

      case 1 => val s = decorate(moveAction(violationTarget)(g), 2)
        assert(s.nonEmpty)
        s
      case 2 => val s = decorate(epsilon(g) ++ absIntro(violationTarget)(g), 3)
        assert(s.nonEmpty)
        s
      case 3 => val s = decorate(redirectTowardAbstractions(violationTarget)(g),4)
        assert(s.nonEmpty)
        s
      case _ => Seq()
    }
  }
}

class TargetedControlWithHeuristic
(val rules: TransformationRules,
 val initialGraph: DependencyGraph,
 val constraints: ConstraintsMaps,
 val violationTarget : ConcreteNode
) extends SearchControl[DecoratedGraph[AutomataState]]
  with Heuristic
  with CheckViolation {

  def initialState: DecoratedGraph[AutomataState] = (initialGraph, 0)

  def nextStates(state : DecoratedGraph[AutomataState]) : Seq[LoggedTry[DecoratedGraph[AutomataState]]] =
    if(!isViolationTarget(state._1, violationTarget.id)) Seq()
    else nextStates(violationTarget)(state._1, state._2)


}