package puck.gui.svg

import javax.swing.JOptionPane

import puck.graph.{DependencyGraph, LoggedTry}

import scalaz.{\/-, -\/}

package object actions {

  def showInputDialog(msg : String) : Option[String] = {
    val childName = JOptionPane.showInputDialog(msg)
    if (childName == null || childName.isEmpty) None
    else Some(childName)
  }

  def printErrOrPushGraph
  ( controller: SVGController, msg : String )
  ( lgt : LoggedTry[DependencyGraph]) : Unit = {
    controller.console.appendText(lgt.log)
    lgt.value match {
      case -\/(err) =>
        controller.console.appendText(s"$msg\n${err.getMessage}\nLog : ${lgt.log}")
      case \/-(g) => controller.pushGraph(g)
    }
  }

}
