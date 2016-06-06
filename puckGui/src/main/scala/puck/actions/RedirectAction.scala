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

import puck.graph.ShowDG._
import puck.graph._
import puck.graph.transformations.rules.Redirection

import scala.swing.{Action, Publisher}

class DisplayableAbstraction(val abs : Abstraction, graph : DependencyGraph) {

  override def toString : String = (graph, abs).shows

}

class RedirectAction0
( controller : Publisher,
  edge : NodeIdP,
  abstractions : Seq[Abstraction])
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
  extends Action("Use abstraction instead"){
  def apply() : Unit = {
    println("Choose Abstraction !" + abstractions)

    val dAbstractions = abstractions map (new DisplayableAbstraction(_, graph))

    Choose( "Redirect toward abtraction",
      s"Choose which abstraction to use instead of ${(graph, edge.target).shows}",
      dAbstractions) match {
      case None =>()
      case Some(dAbs) =>
        printErrOrPushGraph(controller,"Redirection Action failure"){
          Redirection.redirectUsesAndPropagate(graph.mileStone, edge, dAbs.abs)
        }
    }
  }
}



class RedirectAction
(controller : Publisher,
 edge : NodeIdP,
 abs : Abstraction)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
  extends Action(s"Use $abs instead of ${(graph, edge.target).shows}"){

  //TODO check keepOldUse and propagate redirection value
  def apply() : Unit =
    printErrOrPushGraph(controller,"Redirection Action failure"){
      Redirection.redirectUsesAndPropagate(graph.mileStone, edge, abs)
    }
}
