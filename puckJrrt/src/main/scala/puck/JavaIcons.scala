package puck

import java.io.File
import javax.swing.{Icon, ImageIcon}

import puck.graph.{AGRoot, NodeKind}
import puck.gui.explorer.DGTreeIcons
import puck.javaGraph.nodeKind._

/**
  * Created by lorilan on 05/01/16.
  */
object JavaIcons extends DGTreeIcons {

  def iconDirectory : String = "/home/lorilan/projects/constraintsSolver/puckJrrt/src/main/resources/icons/"
  /*getClass.getResource()*/

  val classIcon : Icon = new ImageIcon(iconDirectory + File.separator + "class.gif")
  val rootIcon : Icon = new ImageIcon(iconDirectory + File.separator + "root.gif")
  val interfaceIcon : Icon = new ImageIcon(iconDirectory + File.separator + "interface.gif")
  val methodIcon : Icon = new ImageIcon(iconDirectory + File.separator + "method.png")
  val packageIcon : Icon = new ImageIcon(iconDirectory + File.separator + "package.gif")
  val fieldIcon : Icon = new ImageIcon(iconDirectory + File.separator + "field.png")
  val unknownIcon : Icon = new ImageIcon(iconDirectory + File.separator + "question-mark.gif")

  def iconOfKind(k: NodeKind): Icon = k match {
    case Interface => interfaceIcon
    case Class => classIcon
    case _ : MethodKind => methodIcon
    case Field | StaticField => fieldIcon
    case _ : AGRoot => rootIcon
    case Package => packageIcon
    case _ => unknownIcon
  }
}
