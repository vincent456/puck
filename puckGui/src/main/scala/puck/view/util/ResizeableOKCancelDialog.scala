package puck.view.util

import javax.swing.JOptionPane

import scala.swing.BorderPanel.Position
import scala.swing.Dialog.Result
import scala.swing._

/**
  * Created by Loïc Girault on 12/15/16.
  */
class ResizeableOKCancelDialog
( c : Component) extends Dialog {

  var res : Int = JOptionPane.CLOSED_OPTION

  contents = new BorderPanel {

    add(c, Position.Center)

    add(new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += Button("OK"){
        res = JOptionPane.YES_OPTION
        close()
      }
      contents += Swing.HGlue
      contents += Button("Cancel"){
        res = JOptionPane.CANCEL_OPTION
        close()
      }
      contents += Swing.HGlue
    }, Position.South)
  }
  modal = true
  centerOnScreen()
  open()
}

object ResizeableOKCancelDialog {
  def apply(c : Component) : Result.Value = {
    val d = new ResizeableOKCancelDialog(c)
    Dialog.Result(d.res)
  }
}