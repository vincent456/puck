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

import puck.graph.{DependencyGraph, GraphUtils}
import puck.gui.explorer.{ForbiddenDependenciesExplorer, NodeInfosPanel}
import puck.util.PuckLog

import scala.swing._
import java.awt.Dimension

import puck.graph.constraints.ConstraintsMaps

object PuckMainPanel{
  val width = 1024
  val height = 768


}

abstract class ViewHandler {
  def installView(mainPanel: PuckMainPanel, nodeKindIcons: NodeKindIcons) : Publisher
}

class PuckMainPanel(graphUtils: GraphUtils,
                    val nodeKindIcons : NodeKindIcons)
  extends SplitPane(Orientation.Horizontal) {

  dividerSize = 3
  preferredSize = new Dimension(PuckMainPanel.width, PuckMainPanel.height)

  val console = new ConsoleWithSelection()
  val logger = new TextAreaLogger(console.textArea, PuckLog.verbose)

  val control = PuckControl(graphUtils, nodeKindIcons, logger)

  val interface = new PuckInterfacePanel(control)

  val nodeInfos = new NodeInfosPanel(control.Bus, control)(nodeKindIcons)


  object upPanel extends SplitPane(Orientation.Vertical) {
    leftComponent = interface
    def setGraphView(c : Component) : Unit = {
      rightComponent = new SplitPane(Orientation.Vertical) {
        resizeWeight = 0.25
        leftComponent = c
        rightComponent = nodeInfos
      }
    }
  }

  object downPanel extends SplitPane(Orientation.Vertical) {
    rightComponent = console
    dividerSize = 0

    def setConstraintViolationExplorer
    ( graph : DependencyGraph, constraints : Option[ConstraintsMaps]) : Unit = {

      val violations = constraints match {
        case None => Seq()
        case Some(cm) => (graph, cm).violations
      }

      if (violations.isEmpty) {
        orientation = Orientation.Horizontal
        leftComponent = new Label("0 violation !")
        resizeWeight = 0
        dividerSize = 0
      }
      else {
        orientation = Orientation.Vertical
        leftComponent = new BoxPanel(Orientation.Vertical) {
          contents += new Label("Constraints Violations")
          val constraintViolationExplorer =
            new ForbiddenDependenciesExplorer(control,
              violations, constraints.get)(graph,
              nodeKindIcons)
          contents += constraintViolationExplorer
        }
        resizeWeight = 0.5
        dividerSize = 3
      }
      repaint()

    }
  }

  leftComponent = upPanel
  rightComponent = downPanel

  this listenTo control.Bus

  var currentView : Publisher = TreeViewHandler.installView(PuckMainPanel.this, nodeKindIcons)

  reactions += {
    case SwitchView(handler) =>
      currentView deafTo control.Bus
      currentView = handler.installView(this, nodeKindIcons)
      control.Bus publish GraphUpdate(control.graph)
  }
}

