package puck.view.util

import java.awt._
import java.awt.geom.{Area, RoundRectangle2D}
import javax.swing.border.AbstractBorder

class BubbleBorder
( color: Color,
  thickness : Double,
  radii : Double ) extends AbstractBorder {

  val stroke = new BasicStroke(thickness.toFloat)
  val strokePad : Double = thickness / 2d
  val hints = new RenderingHints (
    RenderingHints.KEY_ANTIALIASING,
    RenderingHints.VALUE_ANTIALIAS_ON)

  val insets : Insets = {
    val pad = radii.toInt + strokePad.toInt
    new Insets(pad, pad, pad, pad)
  }


  def this(color : Color) =  this(color, 4, 8)

  override def getBorderInsets(c : Component) : Insets= insets

  override def getBorderInsets(c : Component, insets: Insets) : Insets =
    getBorderInsets(c)

  override def paintBorder(c : Component, g : Graphics,
                           x : Int, y : Int, width : Int, height : Int) = {

    val g2 = g.asInstanceOf[Graphics2D]

    val bottomLineY = height - thickness

    val bubble = new RoundRectangle2D.Double(
      0d + strokePad,
      0d + strokePad,
      width.toDouble - thickness,
      bottomLineY,
      radii,
      radii)

    g2.setRenderingHints(hints)

    // Paint the BG color of the parent, everywhere outside the clip
    // of the text bubble.
    Option(c.getParent).foreach {
      parent =>
        val bg = parent.getBackground
        val rect = new Rectangle(0,0,width, height)
        val borderRegion = new Area(rect)
        g2.setClip(borderRegion)
        g2.setColor(bg)
        g2.fillRect(0, 0, width, height)
        g2.setClip(null)
    }

    g2.setColor(color)
    g2.setStroke(stroke)
    g2.draw(bubble)
  }
}
