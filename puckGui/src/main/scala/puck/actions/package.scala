package puck

import javax.swing.JOptionPane

import puck.graph._
import puck.gui.svg.SVGController

import scalaz.{\/-, -\/}

/**
  * Created by lorilan on 15/12/15.
  */
package object actions {

  def showInputDialog(msg : String) : Option[String] = {
    val childName = JOptionPane.showInputDialog(msg)
    if (childName == null || childName.isEmpty) None
    else Some(childName)
  }

  def printErrOrPushGraph
  ( controller: GraphStack, msg : String )
  ( lgt : LoggedTry[DependencyGraph]) : Unit = {
    controller.logger.writeln(lgt.log)
    lgt.value match {
      case -\/(err) =>
        controller.logger.writeln(s"$msg\n${err.getMessage}\nLog : ${lgt.log}")
      case \/-(g) => controller.pushGraph(g)
    }
  }
}
