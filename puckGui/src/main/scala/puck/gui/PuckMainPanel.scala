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

import puck.graph.GraphUtils
import puck.gui.explorer.{NodeInfosPanel, DGTreeIcons}
import puck.util.PuckLog

import scala.swing._
import java.awt.Dimension


object PuckMainPanel{
  val width = 1024
  val height = 768


}

abstract class ViewHandler {
  def switchView(mainPanel: PuckMainPanel, treeIcons: DGTreeIcons) : Unit
}

class PuckMainPanel(graphUtils: GraphUtils,
                    val treeIcons : DGTreeIcons)
  extends SplitPane(Orientation.Horizontal) {

  dividerSize = 3
  preferredSize = new Dimension(PuckMainPanel.width, PuckMainPanel.height)

  val console = new ConsoleWithSelection()
  val logger = new TextAreaLogger(console.textArea, PuckLog.verbose)

  val control = PuckControl(graphUtils, logger)

  val interface = new PuckInterfacePanel(control)

  val nodeInfos = new NodeInfosPanel(control.Bus, control)(treeIcons)


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
  }

  leftComponent = upPanel
  rightComponent = downPanel

  var viewHandler : ViewHandler = new TreeViewHandler(this, treeIcons)

  this listenTo control.Bus

  reactions += {
    case SwitchView => viewHandler.switchView(this, treeIcons)
  }
}

