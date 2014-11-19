package puck.gui.imageDisplay

import java.awt.{Dimension, Toolkit}
import java.io.{File, InputStream}
import javax.imageio.ImageIO
import javax.swing.ImageIcon

import puck.gui.PuckMainPanel

import scala.swing.{Component, Frame, ScrollPane}

/**
 * Created by lorilan on 11/05/14.
 */

object ImageFrame{
  def apply(f : File) : ImageFrame = {
    if (f == null || !f.exists()) {
      throw new Error(f + " does not exist")
    }
    new ImageFrame (ImageIO.read(f))
  }
  def apply(stream : InputStream) : ImageFrame = {
    new ImageFrame (ImageIO.read(stream))
  }
}

class ImageFrame(private val image: java.awt.Image) extends Frame{
  visible = true
  contents = new ScrollPane(Component.wrap(new ScrollablePicture(new ImageIcon(image),2)))
  private val screenSize = Toolkit.getDefaultToolkit.getScreenSize
  setSize()

  def setSize(){
    /*if(image != null) {
      size = new Dimension(scala.math.min(screenSize.getWidth, image.getWidth(null) + 20).toInt,
        scala.math.min(screenSize.getHeight, image.getHeight(null) + 70).toInt)
    }
    else{
      size = new Dimension(PuckMainPanel.width, PuckMainPanel.height)
    }*/

    size = new Dimension(PuckMainPanel.width, PuckMainPanel.height)
  }

}