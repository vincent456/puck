package puck.gui.svg

import java.awt._
import java.awt.event._
import javax.swing._
/**
  * Created by lorilan on 03/12/15.
  */
class ScrollablePicture
( i: ImageIcon,
  val maxUnitIncrement: Int = 1)
  extends JLabel(i)
  with Scrollable
  with MouseMotionListener {

  val missingPicture: Boolean = i == null

  if (missingPicture) {
      setText ("No picture found.")
      setHorizontalAlignment (SwingConstants.CENTER)
      setOpaque (true)
      setBackground (Color.white)
  }
  setAutoscrolls (true)
  addMouseMotionListener(this)




  def mouseMoved (e: MouseEvent) : Unit = ()

  def mouseDragged (e: MouseEvent) : Unit =
    scrollRectToVisible(new Rectangle (e.getX, e.getY, 1, 1))


  override def getPreferredSize: Dimension =
    if (missingPicture) new Dimension (320, 480)
    else super.getPreferredSize

  def getPreferredScrollableViewportSize: Dimension = getPreferredSize


  def getScrollableUnitIncrement (visibleRect: Rectangle, orientation: Int, direction: Int): Int = {
    val currentPosition: Int =
      if (orientation == SwingConstants.HORIZONTAL) visibleRect.x
      else visibleRect.y

    if (direction < 0) {
      val newPosition: Int = currentPosition - (currentPosition / maxUnitIncrement) * maxUnitIncrement
      if (newPosition == 0 ) maxUnitIncrement else newPosition
    }
    else
       ((currentPosition / maxUnitIncrement) + 1) * maxUnitIncrement - currentPosition

  }

  def getScrollableBlockIncrement (visibleRect: Rectangle, orientation: Int, direction: Int) : Int =
    if (orientation == SwingConstants.HORIZONTAL)
        visibleRect.width - maxUnitIncrement
    else visibleRect.height - maxUnitIncrement



  def getScrollableTracksViewportWidth: Boolean = false

  def getScrollableTracksViewportHeight: Boolean = false

//  def setMaxUnitIncrement (pixels: Int) : Unit =
//    maxUnitIncrement = pixels

}
