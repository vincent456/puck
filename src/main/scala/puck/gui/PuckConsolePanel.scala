package puck.gui

import scala.swing._

class PuckConsolePanel

  extends BoxPanel(Orientation.Vertical)  {

/*  val lines = 10
  val charPerLine = 50*/
  val console = new TextArea()
  console.editable = false

  contents += new ScrollPane(console)

  contents += new Button() {
    tooltip = "Clear the console"

    action = new Action("Clear"){ def apply() : Unit = {
      console.text = ""
    }
    }
  }
}
