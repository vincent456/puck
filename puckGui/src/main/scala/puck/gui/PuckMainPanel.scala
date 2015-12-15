package puck.gui

import puck.graph.{DependencyGraph, GraphUtils}
import puck.graph.io.FilesHandler
import puck.gui.explorer.ConstraintViolationExplorer

import scala.swing._
import java.awt.Dimension


object PuckMainPanel{
  val width = 1024
  val height = 768

  def leftGlued(c : Component) : BoxPanel = {
    new BoxPanel(Orientation.Horizontal) {
      contents += c
      contents += Swing.HGlue
    }
  }
}

class PuckMainPanel(filesHandler: FilesHandler,
                     graphUtils: GraphUtils)
  extends SplitPane(Orientation.Horizontal){
  dividerSize = 3

  preferredSize = new Dimension(PuckMainPanel.width, PuckMainPanel.height)

  val consolePanel = new PuckConsolePanel()
  val logger = new TextAreaLogger(consolePanel.textArea, filesHandler.logPolicy)

  val interface = new PuckInterfacePanel(logger, filesHandler, graphUtils)
  leftComponent = interface
  rightComponent = consolePanel

  def updateRightComponent(g : DependencyGraph) : Unit = {
    val violations = g.violations()
    if(violations.isEmpty) rightComponent = consolePanel
    else {
      rightComponent = new SplitPane(Orientation.Vertical){
        resizeWeight = 0.5
        leftComponent = new ConstraintViolationExplorer(g, violations)
        rightComponent = consolePanel
      }
    }
    this.repaint()
  }

  this listenTo interface.control
  reactions += {
    case ge : GraphEvent => updateRightComponent(ge.graph)
  }



}

