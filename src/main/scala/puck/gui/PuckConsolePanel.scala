package puck.gui

import puck.graph._
import puck.util.{PuckLogger, PuckLog}

import scala.swing._

class PuckConsolePanel(val filesHandler: FilesHandler)
  extends BoxPanel(Orientation.Vertical)  {

/*  val lines = 10
  val charPerLine = 50*/
  val console = new TextArea()
  console.editable = false

  class ConsoleLogger(val askPrint : PuckLog.Verbosity => Boolean) extends PuckLogger {

    def writeln(msg : => Any)(implicit v : PuckLog.Verbosity) : Unit = {
      if(mustPrint(v)) {
        console.append(preMsg(v) + msg)
        console.append(System.lineSeparator())
      }
    }
    def write(msg : => Any)(implicit v : PuckLog.Verbosity) : Unit = {
      if(mustPrint(v)) {
        console.append(preMsg(v) + msg)
      }
    }
  }

  filesHandler.logger = new ConsoleLogger(filesHandler.logPolicy)

  contents += new ScrollPane(console)

  contents += new Button() {
    tooltip = "Clear the console"
    /*minimumSize =
    maximumSize = minimumSize
    preferredSize = minimumSize*/

    action = new Action("Clear"){ def apply() : Unit = {
      console.text = ""
    }
    }
  }
}
