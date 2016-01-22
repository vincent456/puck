package puck

import javax.swing.{Icon, ImageIcon}

import puck.graph.{AGRoot, NodeKind}
import puck.gui.explorer.DGTreeIcons
import puck.javaGraph.nodeKind._

/**
  * Created by lorilan on 05/01/16.
  */
object JavaIcons extends DGTreeIcons {

  val path = "/icons"

  val classIcon : Icon = new ImageIcon(getClass.getResource(s"$path/class.gif"))
  val interfaceIcon : Icon = new ImageIcon(getClass.getResource(s"$path/interface.gif"))

  val innerClassIcon : Icon = new ImageIcon(getClass.getResource(s"$path/class_inner.png"))
  val innerInterfaceIcon : Icon = new ImageIcon(getClass.getResource(s"$path/interface_inner.png"))

  val rootIcon : Icon = new ImageIcon(getClass.getResource(s"$path/root.gif"))

  val constructorIcon : Icon = new ImageIcon(getClass.getResource(s"$path/constructor.png"))

  val methodIcon : Icon = new ImageIcon(getClass.getResource(s"$path/method.png"))
  val abstractMethodIcon : Icon = new ImageIcon(getClass.getResource(s"$path/method_abstract.png"))
  val staticMethodIcon : Icon = new ImageIcon(getClass.getResource(s"$path/method_static.png"))

  val packageIcon : Icon = new ImageIcon(getClass.getResource(s"$path/package.gif"))
  val fieldIcon : Icon = new ImageIcon(getClass.getResource(s"$path/field.png"))
  val staticFieldIcon : Icon = new ImageIcon(getClass.getResource(s"$path/field_static.png"))

  val unknownIcon : Icon = new ImageIcon(getClass.getResource(s"$path/question-mark.gif"))

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
