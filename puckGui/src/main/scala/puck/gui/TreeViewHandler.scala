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

package puck.gui


import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.gui.explorer.{GraphExplorer, ConstraintViolationExplorer}
import puck.gui.svg.SVGViewHandler

import scala.swing._

/**
  * Created by Loïc Girault on 26/01/16.
  */
class TreeViewHandler
  (mainPanel : PuckMainPanel,
   implicit val treeIcons : NodeKindIcons)
  extends ViewHandler with Publisher {

  def switchView(mainPanel: PuckMainPanel, treeIcons: NodeKindIcons) : Unit = {
    this deafTo mainPanel.control.Bus
    mainPanel.viewHandler = new SVGViewHandler(mainPanel)
  }

  import mainPanel.control

  val graphExplorer = new GraphExplorer(control)

  this listenTo control.Bus

  reactions += {
    case _: GraphStackEvent => update(control.graph, control.constraints)
    case ConstraintsUpdate(g, cm) => update(g, Some(cm))
  }


  mainPanel.upPanel.setGraphView(graphExplorer)



  import mainPanel.downPanel
  def update(graph : DependencyGraph, constraints : Option[ConstraintsMaps]) : Unit = {
    val violations = constraints match {
      case None => Seq()
      case Some(cm) => (graph, cm).violations()
    }

    if (violations.isEmpty) {
      downPanel.orientation = Orientation.Horizontal
      downPanel.leftComponent = new Label("0 violation !")
      downPanel.resizeWeight = 0
      downPanel.dividerSize = 0
    }
    else {
      downPanel.orientation = Orientation.Vertical
      downPanel.leftComponent = new BoxPanel(Orientation.Vertical) {
        contents += new Label("Constraints Violations")
        val constraintViolationExplorer =
          new ConstraintViolationExplorer(control.Bus, violations,
            control.printingOptionsControl, constraints.get)(graph,
            control.graphUtils,
            treeIcons)
        contents += constraintViolationExplorer
      }
      downPanel.resizeWeight = 0.5
      downPanel.dividerSize = 3
    }
    downPanel.repaint()

  }


}

