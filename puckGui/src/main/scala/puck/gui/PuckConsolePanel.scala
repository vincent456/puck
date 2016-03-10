/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

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