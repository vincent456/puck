package puck

import javax.swing.JOptionPane

import puck.graph._
import puck.gui.PrintErrOrPushGraph

import scala.swing.Publisher

/**
  * Created by lorilan on 15/12/15.
  */
package object actions {

  def showInputDialog(msg : String, initialValue: String = "") : Option[String] = {
    val childName = JOptionPane.showInputDialog(msg, initialValue)
    if (childName == null || childName.isEmpty) None
    else Some(childName)
  }

  def printErrOrPushGraph
  (publisher : Publisher, msg : String )
  ( lgt : LoggedTry[DependencyGraph]) : Unit =
    publisher publish PrintErrOrPushGraph(msg, lgt)


}
