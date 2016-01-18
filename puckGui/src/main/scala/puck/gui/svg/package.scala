package puck.gui

import java.awt.event.ActionEvent
import java.io.File
import javax.swing._

package object svg {


  def checkBox(name: String, initiallySelected : Boolean)(f: Boolean => Unit) : JCheckBox = {
    val checkBox: JCheckBox = new JCheckBox
    checkBox.setSelected(initiallySelected)
    checkBox.setAction(new AbstractAction(name) {
      def actionPerformed(e: ActionEvent) : Unit = f(checkBox.isSelected)
    })
    checkBox
  }



  def jbutton(name:String)
             (action : ActionEvent => Unit) : JButton =
  new JButton(abstractAction(name)(action))

}
