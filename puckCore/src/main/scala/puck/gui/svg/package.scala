package puck.gui

import java.awt.event.ActionEvent
import java.io.File
import javax.swing._

package object svg {
  implicit class JPopupSyntax(val menu : JPopupMenu) extends AnyVal {
    def addMenuItem(name : String)(action : ActionEvent => Unit) : JMenuItem = {
      menu.add(abstractAction(name)(action))
    }
  }

  def abstractAction(name:String)
                    (action : ActionEvent => Unit) : AbstractAction =
    new AbstractAction(name){
      def actionPerformed(e: ActionEvent) : Unit = action(e)
    }

  def jbutton(name:String)
             (action : ActionEvent => Unit) : JButton =
  new JButton(abstractAction(name)(action))

  private def chooseFile
  ( currentDir : File,
    chooserMode : JFileChooser => Int) : Option[File] = {
    val chooser = new JFileChooser()
    chooser.setCurrentDirectory(currentDir)
    val returnVal: Int = chooserMode(chooser)
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      Some(chooser.getSelectedFile)
    }
    else None
  }

  def openFile(currentDir : File, parent : java.awt.Component) : Option[File] =
    chooseFile(currentDir, chooser => chooser.showOpenDialog(parent) )
  def saveFile(currentDir : File, parent : java.awt.Component) : Option[File] =
    chooseFile(currentDir, chooser => chooser.showSaveDialog(parent) )

}
