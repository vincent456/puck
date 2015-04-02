package puck.gui.svg

import javax.swing.JOptionPane

/**
 * Created by lorilan on 4/2/15.
 */
package object actions {

  def showInputDialog(msg : String) : Option[String] = {
    val childName = JOptionPane.showInputDialog(msg)
    if (childName == null || childName.isEmpty) None
    else Some(childName)
  }

}
