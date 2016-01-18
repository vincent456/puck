package puck

import java.awt.MouseInfo
import java.awt.event.{ActionEvent, MouseEvent}
import java.io.File
import javax.swing.{JFileChooser, AbstractAction, JMenuItem, JPopupMenu}

import scala.swing.{Swing, Orientation, BoxPanel, Component}

/**
  * Created by lorilan on 17/12/15.
  */
package object gui {
  def isRightClick(e : MouseEvent) : Boolean = {
    MouseInfo.getNumberOfButtons > 2 && e.getButton == MouseEvent.BUTTON3 ||
      MouseInfo.getNumberOfButtons == 2 && e.getButton == MouseEvent.BUTTON2
  }

  def leftGlued(c : Component) : BoxPanel = {
    new BoxPanel(Orientation.Horizontal) {
      contents += c
      contents += Swing.HGlue
    }
  }

  implicit class LeftGlued(val c : Component) extends AnyVal {
    def leftGlued : BoxPanel = gui.leftGlued(c)
  }

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
