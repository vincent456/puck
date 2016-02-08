package puck.gui

import puck.graph.io.FilesHandler
import puck.util.FileHelper.FileOption

import scala.swing._
import java.awt.Dimension

class SettingsFrame(filesHandler : FilesHandler) extends Frame{

  title = "Settings"
  size = new Dimension(300, 150)

  contents =  new BoxPanel(Orientation.Vertical){

    def makeFileSelectionLine(title : String,
                              tip : String,
                              fo : FileOption) = {
      val path : Label = new Label(fo.get match {
        case None => "None"
        case Some(f) => f.toString
      })

      val hbox = new Button(title){

        tooltip = tip

        action = new Action(title){
          def apply() : Unit = {
            val fc = new FileChooser(filesHandler.srcDirectory !)
            fc.title = title
            fc.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
            fc showDialog(null, "Select")
            val f = fc.selectedFile
            if(f != null) {
              fo set Some(f)
              path.text = f.getPath
            }
          }
        }
      }.leftGlued
      hbox.contents += path
      hbox
    }

    contents += makeFileSelectionLine("Dot", "",
      filesHandler.graphvizDot)

    contents += makeFileSelectionLine("Editor", "",
      filesHandler.editor)

    contents += makeFileSelectionLine("Decouple",
      "Select the file containing the decoupling constraints",
      filesHandler.decouple)

    contents += makeFileSelectionLine("Jar list file",
      "Select a file containing a list of the jar libraries required by the analysed program",
      filesHandler.jarListFile)
  }
}
