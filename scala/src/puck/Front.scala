package puck

import scala.swing._
import javax.swing.UIManager
import puck.gui.PuckControlPanel

/**
 * Created by lorilan on 08/05/14.
 */
/*object Front extends SwingApplication{

  override def startup(args: Array[String]){
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    if (top.size == new Dimension(0,0)) top.pack()
    top.visible = true
  }
  def top = new MainFrame {
    title = "Puck"

    size = new Dimension(300, 200)

    contents = new PuckControlPanel(FilesHandler("/home/lorilan/puck_svn/distrib/examples/bridge/hannemann_inspired/candidate")())
  }
}*/

object Front{
  def main(args : Array[String]){
    val fh = FilesHandler("/home/lorilan/puck_svn/distrib/examples/bridge/hannemann_inspired/candidate")()
    fh.loadGraph(null)
    println("graph loaded")
    println("make dot ...")
    fh.makeDot()
    println("dot to png...")
    fh.dot2png()
  }
}