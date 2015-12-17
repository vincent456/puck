package puck

import java.awt.MouseInfo
import java.awt.event.MouseEvent

/**
  * Created by lorilan on 17/12/15.
  */
package object gui {
  def isRightClick(e : MouseEvent) : Boolean = {
    MouseInfo.getNumberOfButtons > 2 && e.getButton == MouseEvent.BUTTON3 ||
      MouseInfo.getNumberOfButtons == 2 && e.getButton == MouseEvent.BUTTON2

  }
}
