package puck.gui

import puck.{GraphStack, StackListener}
import puck.graph.GraphUtils
import puck.graph.io.FilesHandler
import puck.gui.explorer.{DGTreeIcons, GraphExplorer, ConstraintViolationExplorer}

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

  implicit class LeftGlued(val c : Component) extends AnyVal {
    def leftGlued : BoxPanel = PuckMainPanel.leftGlued(c)
  }
}

class PuckMainPanel(filesHandler: FilesHandler,
                     graphUtils: GraphUtils,
                    treeIcons : DGTreeIcons)
  extends SplitPane(Orientation.Horizontal) with StackListener{
  dividerSize = 3

  preferredSize = new Dimension(PuckMainPanel.width, PuckMainPanel.height)

  val consolePanel = new PuckConsolePanel()
  val logger = new TextAreaLogger(consolePanel.textArea, filesHandler.logPolicy)

  val interface = new PuckInterfacePanel(logger, filesHandler, graphUtils,treeIcons)
  leftComponent = interface
  rightComponent = consolePanel

  def update(svgController: GraphStack) : Unit= {

    val violations = svgController.graph.violations()
    if(violations.isEmpty) rightComponent = consolePanel
    else {
      rightComponent = new SplitPane(Orientation.Vertical){
        resizeWeight = 0.5
        leftComponent = new BoxPanel(Orientation.Vertical) {
          contents += new Label("Constraints Violations")
          val vExplorer = new ConstraintViolationExplorer(interface.control, violations)
          contents += vExplorer
          interface listenTo vExplorer
        }
        rightComponent = consolePanel
      }
    }
    this.repaint()
  }

  interface.control.registerAsStackListeners(this)

}

