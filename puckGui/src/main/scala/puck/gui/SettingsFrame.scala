package puck.gui

import puck.graph.io.Project

import scala.swing._
import java.awt.Dimension

class SettingsFrame(project : Project) extends Frame{

  title = "Settings"
  size = new Dimension(300, 150)

  contents =  new BoxPanel(Orientation.Vertical){

    def makeFileSelectionLine
      ( title : String,
        tip : String,
        k : Project.FileKey )  = {

      val path : Label = new Label(project(k) match {
        case None => "None"
        case Some(f) => f.toString
      })

      val hbox = new Button(title){

        tooltip = tip

        action = new Action(title){
          def apply() : Unit = {
            val fc = new FileChooser(project.workspace)
            fc.title = title
            fc.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
            fc showDialog(null, "Select")
            val f = fc.selectedFile
            if(f != null) {
              project set (k, f)
              path.text = f.getPath
            }
          }
        }
      }.leftGlued
      hbox.contents += path
      hbox
    }

    import Project.Keys

    contents += makeFileSelectionLine("Dot", "",
      Keys.dotPath)

    contents += makeFileSelectionLine("Editor", "",
      Keys.editor)

    contents += makeFileSelectionLine("Decouple",
      "Select the file containing the decoupling constraints",
      Keys.decouple)

//    contents += makeFileSelectionLine("Jar list file",
//      "Select a file containing a list of the jar libraries required by the analysed program",
//      filesHandler.jarListFile)
  }
}
