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

class ConsoleWithSelection
( val lines: Int = 10,
  val charPerLine: Int = 50)
  extends SplitPane(Orientation.Horizontal) {

  dividerSize = 0
  resizeWeight = 0.2

  private val selection: Label = new Label

  val console = new PuckConsolePanel(){
    selection +=: contents
    textArea.rows = lines
    textArea.columns = charPerLine
  }

  topComponent = selection
  bottomComponent = console

  def textArea = console.textArea

  def displaySelection(node: String) : Unit = {
    if (node.length > 0) selection.text = "Selection : " + node
    else selection.text = "No selection"
  }

  def appendText(txt: String) : Unit = {
    textArea.text = textArea.text + txt + "\n"
  }
}