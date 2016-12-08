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

package puck.view


import puck.control.{ConstraintsUpdate, GraphStackEvent}
import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.view.explorer.GraphExplorer

import scala.swing._

/**
  * Created by Loïc Girault on 26/01/16.
  */

object TreeViewHandler extends ViewHandler {
  override def toString = "Tree View"

  def installView(mainPanel: PuckMainPanel, treeIcons: NodeKindIcons) : Publisher = {
    new TreeViewHandler(mainPanel,
      new GraphExplorer(mainPanel.control, treeIcons))

  }
}

class TreeViewHandler
  (mainPanel : PuckMainPanel,
   graphExplorer : Component)
  extends Publisher {

  import mainPanel.control


  this listenTo control.Bus

  reactions += {
    case _: GraphStackEvent => update(control.graph, control.constraints)
    case ConstraintsUpdate(g, cm) => update(g, Some(cm))
  }


  mainPanel.upPanel.setGraphView(graphExplorer)

  def update(graph : DependencyGraph, constraints : Option[ConstraintsMaps]) : Unit =
    mainPanel.downPanel.setConstraintViolationExplorer(graph, constraints)

}

