package puck.view.util

import javax.swing.ImageIcon

import scala.swing.event.MouseClicked
import scala.swing.{BoxPanel, Label, Orientation, Swing}

/**
  * Created by Loïc Girault on 12/15/16.
  */
abstract class LabelImageHGlued(val text : String, img : java.net.URL)
  extends BoxPanel(Orientation.Horizontal){
  def action(mc : MouseClicked) : Unit
  contents += new Label(text)
  contents += Swing.HGlue
  contents += new Label {
    icon = new ImageIcon(img)
    listenTo(mouse.clicks)
    reactions += {
      case mc@MouseClicked(_, _, _, _, _) => action(mc)
    }
  }
}
