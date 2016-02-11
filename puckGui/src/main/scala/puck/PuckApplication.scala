package puck

import java.awt.Dimension
import javax.swing.UIManager

import puck.graph.GraphUtils
import puck.gui.PuckMainPanel
import puck.gui.explorer.DGTreeIcons

import scala.swing.{MainFrame, SwingApplication}

class PuckApplication
  (gu : GraphUtils,
   treeIcons : DGTreeIcons)
  extends SwingApplication{

  def startup(args: Array[String]) : Unit = {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    if (top.size == new Dimension(0,0)) top.pack()
    top.visible = true
  }

  val top = new MainFrame {
    title = "Puck"
    contents  = new PuckMainPanel(gu, treeIcons)
  }
}
