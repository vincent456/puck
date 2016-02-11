package puck.gui

import puck.graph.GraphUtils
import puck.graph.io.Project
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

  val control = new PuckControl(logger, graphUtils)

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

