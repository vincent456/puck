package puck.gui

import java.io.File
import puck.graph.{FilesHandler, NodeKind}

import scala.swing._
import java.awt.Dimension

/**
 * Created by lorilan on 09/05/14.
 */
class SettingsFrame[Kind <: NodeKind[Kind]](filesHandler : FilesHandler[Kind]) extends Frame{

  title = "Settings"
  size = new Dimension(300, 150)

  contents =  new BoxPanel(Orientation.Vertical){

    def makeFileSelectionLine(title : String,
                              tip : String,
                              initValue : Option[File])
                             (setter : File => Unit) = {
      val path : Label = new Label(initValue match {
        case None => "None"
        case Some(f) => f.toString
      })

      val hbox = PuckMainPanel.leftGlued(new Button(title){

        tooltip = tip

        action = new Action(title){
          def apply() {
            val fc = new FileChooser(filesHandler.srcDirectory.get)
            fc.title = title
            fc.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
            fc showDialog(null, "Select")
            val f = fc.selectedFile
            if(f != null) {
              setter(f)
              path.text = f.getPath
            }
          }
        }
      })
      hbox.contents += path
      hbox
    }

    contents += makeFileSelectionLine("Dot", "",
      filesHandler.graphvizDot){ f =>
      filesHandler.graphvizDot = Some(f)
    }
    contents += makeFileSelectionLine("Editor", "",
      filesHandler.editor){ f =>
      filesHandler.editor = Some(f)
    }

    contents += makeFileSelectionLine("Decouple",
      "Select the file containing the decoupling constraints",
      filesHandler.decouple){ f =>
      filesHandler.decouple = Some(f)
    }

    contents += makeFileSelectionLine("Jar list file",
      "Select a file containing a list of the jar libraries required by the analysed program",
      filesHandler.jarListFile){ f =>
      filesHandler.jarListFile = Some(f)
    }
  }
}
