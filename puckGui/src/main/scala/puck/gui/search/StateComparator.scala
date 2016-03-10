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

package puck.gui.search

import puck.graph._
import puck.graph.io.VisibilitySet
import puck.graph.transformations.Transformation
import puck.search.SearchState
import puck.util.PuckFileLogger

import scala.swing.{BoxPanel, Button, Label, Orientation}

class StateComparator
( initialRecord :  Seq[Transformation],
  sortedRes: Map[Int, Seq[SearchState[SResult]]],
  printId : () => Boolean,
  printSig : () => Boolean,
  visibility : VisibilitySet.T)
  extends BoxPanel(Orientation.Vertical) {
  contents += new Label("Compare")
  val cb1 = new StateSelector(sortedRes, printId, printSig, visibility )
  val cb2 = new StateSelector(sortedRes, printId, printSig, visibility )

  this deafTo this

  this listenTo cb1
  this listenTo cb2

  reactions += {
    case e => //println("CSSearchStateComparator : chaining event")
      publish(e)
  }

  contents += cb1
  contents += new Label("and")
  contents += cb2
  contents += Button(">>") {
    val equivalent = DependencyGraph.areEquivalent(initialRecord,
      graphOfResult(cb1.selectedState.loggedResult.value),
      graphOfResult(cb2.selectedState.loggedResult.value), new PuckFileLogger(_ => true,
        new java.io.File("compare_log")))
      //new PuckSystemLogger(_ => true))

    println("equivalent = " + equivalent)

  }
}
