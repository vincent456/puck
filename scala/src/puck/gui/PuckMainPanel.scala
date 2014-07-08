package puck.gui

import scala.swing._
import puck.FilesHandler
import java.awt.Dimension
import java.io.OutputStream

/**
 * Created by lorilan on 08/05/14.
 */

object PuckMainPanel{
  val width = 600
  val height = 800
}

class PuckMainPanel(private val filesHandler: FilesHandler) extends SplitPane(Orientation.Horizontal){
  dividerSize = 3

  preferredSize = new Dimension(PuckMainPanel.width, PuckMainPanel.height)

  val console = new TextArea()
  console.editable = false

  leftComponent = new PuckControlPanel(filesHandler, new OutputStream {
    override def write(p1: Int): Unit = console.append(String.valueOf(p1.toChar))
  })
  /*console.preferredSize = new Dimension(PuckMainPanel.width, PuckMainPanel.height * 1 / 3)
  console.minimumSize = console.preferredSize*/

  rightComponent = new BoxPanel(Orientation.Vertical) {
    contents += new ScrollPane(console)

    contents += new Button(){
      tooltip = "Clear the console"
      /*minimumSize =
      maximumSize = minimumSize
      preferredSize = minimumSize*/

      action = new Action("Clear"){ def apply(){
          console.text = ""
        }
      }
    }
  }



}

