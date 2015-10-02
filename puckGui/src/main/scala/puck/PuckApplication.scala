package puck

import java.awt.Dimension
import javax.swing.UIManager

import puck.graph.GraphUtils
import puck.graph.io.FilesHandler
import puck.gui.PuckMainPanel

import scala.swing.{MainFrame, SwingApplication}

class PuckApplication
  ( fh : FilesHandler,
    gu : GraphUtils)
  extends SwingApplication{

  def startup(args: Array[String]) : Unit = {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    if (top.size == new Dimension(0,0)) top.pack()
    top.visible = true
  }

  def top = new MainFrame {
    title = "Puck"

    contents  = new PuckMainPanel(fh, gu)

  }
}
