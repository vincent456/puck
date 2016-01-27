package puck.gui

import puck.graph.{DependencyGraph, GraphUtils}
import puck.graph.io.FilesHandler
import puck.gui.explorer.{GraphExplorer, NodeInfosPanel, DGTreeIcons, ConstraintViolationExplorer}
import puck.gui.menus.EdgeMenu

import scala.swing._
import java.awt.Dimension


object PuckMainPanel{
  val width = 1024
  val height = 768


}

abstract class ViewHandler {
  def switchView(mainPanel: PuckMainPanel, treeIcons: DGTreeIcons) : Unit
}

class PuckMainPanel(filesHandler: FilesHandler,
                    graphUtils: GraphUtils,
                    treeIcons : DGTreeIcons)
  extends SplitPane(Orientation.Horizontal) {
  dividerSize = 3

  preferredSize = new Dimension(PuckMainPanel.width, PuckMainPanel.height)

  val console = new ConsoleWithSelection()
  val logger = new TextAreaLogger(console.textArea, filesHandler.logPolicy)

  val control = new PuckControl(logger, filesHandler, graphUtils)

  val interface = new PuckInterfacePanel(control)



  val nodeInfos = new ScrollPane(){
    reactions += {
      case NodeClicked(n) if n.id != DependencyGraph.rootId =>
        Swing.onEDT {
          contents =
            new NodeInfosPanel(interface, control.graph, n.id,
              edge => {
                new EdgeMenu(interface, edge,
                  control.printingOptionsControl,
                  blurrySelection = false,
                  control.graphStack.graph,
                  graphUtils)},
              treeIcons
            )
        }

    }
  }

  nodeInfos listenTo control

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
  }

  leftComponent = upPanel
  rightComponent = downPanel

  var viewHandler : ViewHandler = new TreeViewHandler(this, treeIcons)

  this listenTo interface

  reactions += {
    case SwitchView => viewHandler.switchView(this, treeIcons)
  }
}

