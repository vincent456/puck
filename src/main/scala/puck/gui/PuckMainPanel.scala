package puck.gui

import puck.graph.FilesHandler

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

class PuckMainPanel(filesHandler: FilesHandler)
  extends SplitPane(Orientation.Horizontal){
  dividerSize = 3

  preferredSize = new Dimension(PuckMainPanel.width, PuckMainPanel.height)

  leftComponent = new PuckInterfacePanel(filesHandler)

  rightComponent = new PuckConsolePanel(filesHandler)
}

