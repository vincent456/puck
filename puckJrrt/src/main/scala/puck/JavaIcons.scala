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
  val interfaceIcon : Icon = new ImageIcon(iconDirectory + File.separator + "interface.gif")

  val innerClassIcon : Icon = new ImageIcon(iconDirectory + File.separator + "class_inner.png")
  val innerInterfaceIcon : Icon = new ImageIcon(iconDirectory + File.separator + "interface_inner.png")

  val rootIcon : Icon = new ImageIcon(iconDirectory + File.separator + "root.gif")

  val constructorIcon : Icon = new ImageIcon(iconDirectory + File.separator + "constructor.png")

  val methodIcon : Icon = new ImageIcon(iconDirectory + File.separator + "method.png")
  val abstractMethodIcon : Icon = new ImageIcon(iconDirectory + File.separator + "method_abstract.png")
  val staticMethodIcon : Icon = new ImageIcon(iconDirectory + File.separator + "method_static.png")

  val packageIcon : Icon = new ImageIcon(iconDirectory + File.separator + "package.gif")
  val fieldIcon : Icon = new ImageIcon(iconDirectory + File.separator + "field.png")
  val staticFieldIcon : Icon = new ImageIcon(iconDirectory + File.separator + "field_static.png")

  val unknownIcon : Icon = new ImageIcon(iconDirectory + File.separator + "question-mark.gif")

  def iconOfKind(k: NodeKind): Icon = k match {
    case Interface => interfaceIcon
    case Class => classIcon
    case InnerClass => innerClassIcon
    case InnerInterface => innerInterfaceIcon
    case AbstractMethod => abstractMethodIcon
    case StaticMethod => staticMethodIcon
    case Method => methodIcon
    case Field => fieldIcon
    case StaticField => staticFieldIcon
    case Constructor => constructorIcon
    case _ : AGRoot => rootIcon
    case Package => packageIcon
    case _ => unknownIcon
  }
}
