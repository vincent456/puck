package puck.gui

import puck.graph.GraphUtils
import puck.graph.io.FilesHandler
import puck.gui.explorer.{DGTreeIcons, ConstraintViolationExplorer}

import scala.swing._
import java.awt.Dimension


object PuckMainPanel{
  val width = 1024
  val height = 768


}

class PuckMainPanel(filesHandler: FilesHandler,
                     graphUtils: GraphUtils,
                    treeIcons : DGTreeIcons)
  extends SplitPane(Orientation.Horizontal) {
  dividerSize = 3

  preferredSize = new Dimension(PuckMainPanel.width, PuckMainPanel.height)

  val consolePanel = new PuckConsolePanel()
  val logger = new TextAreaLogger(consolePanel.textArea, filesHandler.logPolicy)

  val interface = new PuckInterfacePanel(logger, filesHandler, graphUtils,treeIcons)
  leftComponent = interface
  rightComponent = consolePanel

  reactions += {
  case GraphUpdate(graph) =>
    println("updating main panel")
    val violations = graph.violations()
    if(violations.isEmpty) rightComponent = consolePanel
    else {
      rightComponent = new SplitPane(Orientation.Vertical){
        resizeWeight = 0.5
        leftComponent = new BoxPanel(Orientation.Vertical) {
          contents += new Label("Constraints Violations")
          contents += new ConstraintViolationExplorer(interface, violations,
            () => interface.control.printingOptionsControl.printingOptions)(graph, graphUtils)
        }
        rightComponent = consolePanel
      }
    }
    this.repaint()
  }

  this listenTo interface.control


}

