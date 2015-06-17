package puck.gui

import puck.graph.GraphUtils
import puck.graph.io.FilesHandler

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

  leftComponent = new PuckInterfacePanel(logger, filesHandler, graphUtils)


  rightComponent = consolePanel
}

