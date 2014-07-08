package puck.gui

import scala.swing._
import puck.FilesHandler
import java.awt.Dimension

/**
 * Created by lorilan on 09/05/14.
 */
class SettingsFrame(filesHandler : FilesHandler) extends Frame{

  title = "Settings"
  size = new Dimension(300, 150)

  contents = new FlowPanel{
    var path : Label = _
    contents += new Button("Set dot path"){
      path = new Label(
        filesHandler.graphvizDot match {
          case None => "no path setted: puck will use PATH variable to seek dot"
          case Some(f) => f.getPath
        })

      action = new Action("Choosing dot path"){
          def apply() {
            val fc = new FileChooser
            fc.title = "Select dot path"
            fc.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
            fc showDialog(null, "Select")
            val f = fc.selectedFile
            if(f != null) {
              filesHandler.graphvizDot = f
              path.text = f.getPath
            }
          }
      }
    }
    contents += path
  }
}
