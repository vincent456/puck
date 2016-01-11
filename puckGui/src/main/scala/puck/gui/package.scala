package puck

import java.awt.MouseInfo
import java.awt.event.MouseEvent

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

}
