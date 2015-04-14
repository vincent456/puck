package puck.gui.svg

import javax.swing.JOptionPane

import puck.graph.{DependencyGraph, Try}
import scalaz.{\/-, -\/}
import puck.graph.transformations.rules

package object actions {

  def showInputDialog(msg : String) : Option[String] = {
    val childName = JOptionPane.showInputDialog(msg)
    if (childName == null || childName.isEmpty) None
    else Some(childName)
  }

  def printErrOrPushGraph(controller: SVGController, msg : String) : Try[DependencyGraph] => Unit = {
    case -\/(err) => controller.console.appendText(s"$msg\n${err.getMessage}\n" )
    case \/-(g) => controller.pushGraph(g)
  }

  type CreateVarStrategy = rules.CreateVarStrategy
  val CreateTypeMember = rules.CreateTypeMember
  val CreateParameter = rules.CreateParameter

}
