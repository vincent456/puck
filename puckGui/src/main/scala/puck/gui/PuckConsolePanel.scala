package puck.gui

import scala.swing._

class PuckConsolePanel
  extends BoxPanel(Orientation.Vertical)  {

/*  val lines = 10
  val charPerLine = 50*/
  val textArea = new TextArea()
  textArea.editable = false

  contents += new ScrollPane(textArea)

  contents += new Button() {
    tooltip = "Clear the console"

    action = new Action("Clear"){ def apply() : Unit = {
      textArea.text = ""
    }
    }
  }
}
