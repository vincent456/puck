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

package puck.gui.svg
package actions

import java.awt.Dimension
import javax.swing.SwingUtilities

import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.io.{PrintingOptions, Visible, VisibilitySet}
import VisibilitySet._
import puck.gui._
import puck.gui.search.{StateSelected, SimpleElementSelector, SortedElementSelector}
import puck.search.{SearchState, Search}
import puck.util._

import scala.concurrent.ExecutionContext
import scala.swing.BorderPanel.Position
import scala.swing.TabbedPane.Page
import scala.swing.event.Event
import scala.swing._

import scalaz.syntax.writer._

//this trait is needed to dispatch graphical computation on the rerserved thread in Intellij Idea plugin
trait SwingService {

  implicit val executor : ExecutionContext

  def swingInvokeLater (f : () => Unit ) : Unit
}

object DefaultSwingService extends SwingService {

  implicit val executor : ExecutionContext =
    scala.concurrent.ExecutionContext.Implicits.global

  def swingInvokeLater (f : () => Unit ) : Unit =
    SwingUtilities.invokeLater(new Runnable {
      def run(): Unit = f()
    })
}


case class Log(msg : String) extends Event


trait ResultPanel {
  def selectedResult: Logged[DependencyGraph]
}

trait GraphPanelResultPanel extends ResultPanel {
  val swingService : SwingService = DefaultSwingService
  def printingOptions : PrintingOptions
  val visibilitySet : VisibilitySet.T
  val constraints : ConstraintsMaps
  val graphUtils : GraphUtils
  val publisher : Publisher

  import swingService.{executor, swingInvokeLater}

  def graphPanel
  ( graph : DependencyGraph,
    visibilitySet: VisibilitySet.T) : Component =
    new ScrollPane() {
      documentFromGraph(graph,
        graphUtils,
        Some(constraints),
        printingOptions.copy(visibility = visibilitySet))(
        documentFromGraphErrorMsgGen(
          msg => publisher.publish(Log(msg)))){
        case d =>
          val c = PUCKSVGCanvas()
          swingInvokeLater { () =>
            c.setDocument(d)
            viewportView = Component.wrap(c)
          }
      }
    }

  def selectedResultGraphPanel = {

    import Recording.RecordingOps
    val g = selectedResult.value

    val nodeVisibles  = {
      val involveNodes = g.recording.subRecordFromLastMilestone.involveNodes
      val nns = involveNodes flatMap g.containerPath
      visibilitySet.setVisibility(nns, Visible)
    }

    graphPanel(g, nodeVisibles)
  }
}


class AutosolveResultPanel
( val publisher : Publisher,
  val constraints : ConstraintsMaps,
  violationTarget : ConcreteNode,
  printingOptionsControl: PrintingOptionsControl,
  res : Search[SResult])
( implicit val beforeGraph : DependencyGraph,
  val graphUtils : GraphUtils )
  extends SplitPane(Orientation.Horizontal)
  with GraphPanelResultPanel {

  reactions += {
    case poe : PrintingOptionEvent => poe(printingOptionsControl)
      publish(PrintingOptionsUpdate)
  }

  val visibilitySet = {

    val users = beforeGraph.usersOfExcludingTypeUse(violationTarget.id)

    val targetAndAncestors =
      beforeGraph.containerPath(violationTarget.id)

    val vs = users.foldLeft(targetAndAncestors){
      (s,id) => (beforeGraph.containerPath(id).toSet + id) ++: s
    }

    VisibilitySet.allHidden(beforeGraph).setVisibility(vs, Visible)

  }

  def printingOptions: PrintingOptions =
    printingOptionsControl.printingOptions.copy(visibility = visibilitySet)

  def selectedResult : Logged[DependencyGraph] = activePanel.selectedResult

  val successesTab = 0
  val failuresTab = 1

  def selectorPanelOrDummy
  ( withSelector : Boolean,
    selector : => Component with Selector )  =
    if(withSelector) {
      val p = new SelectorResultPanel(selector, constraints, this)
      p listenTo this
      this listenTo p
      this listenTo p.selector
      p
    }
    else new DummyResultPanel(beforeGraph)

  val successesPanel =
    selectorPanelOrDummy(res.successes.nonEmpty, new SuccessSelector(res))

  val failurePanel =
    selectorPanelOrDummy(res.failures.nonEmpty, new FailureSelector(res))


  val tabs = new TabbedPane() {
    pages += new Page("Success", successesPanel) {
      mnemonic = successesTab
    }
    pages += new Page("Failures", failurePanel) {
      mnemonic = failuresTab
    }
  }


  def activePanel : ResultPanel =
  if (tabs.selection.page.mnemonic == failuresTab) failurePanel
  else successesPanel


  val console = new PuckConsolePanel()
  val upPane =  new SplitPane(Orientation.Vertical) {

    dividerSize = 3
    preferredSize = new Dimension(1024, 780)

    leftComponent = graphPanel(beforeGraph, printingOptions.visibility)

    // val stateSelector = new SortedStateSelector(res.allStatesByDepth)
    rightComponent = tabs

  }

  leftComponent = upPane
  rightComponent = new SplitPane(Orientation.Vertical) {
    leftComponent = new BoxPanel(Orientation.Vertical){
      SVGMenu.addVisibilityCheckBoxes(this, printingOptionsControl)
    }
    rightComponent = console
  }

  reactions += {
    case Log(msg) => console.textArea.text = msg
  }

}




class DummyResultPanel
(val beforeGraph : DependencyGraph)
  extends FlowPanel with ResultPanel {

  def selectedResult : Logged[DependencyGraph] = beforeGraph.set("")


}

trait Selector extends Publisher{
  def selectedResult: Logged[DependencyGraph]
  def selectedLog : String = selectedResult.written
}

class FailureSelector(res : Search[SResult])
  extends SortedElementSelector[SearchState[SResult]](
    res.failuresByDepth, StateSelected.apply) with Selector{

  assert(res.failures.nonEmpty)
  def selectedResult = selectedState.prevState.get.success map graphOfResult
  override def selectedLog = selectedState.loggedResult.log
}

class SuccessSelector(res : Search[SResult])
  extends SimpleElementSelector[SearchState[SResult]](StateSelected.apply) with Selector{
  assert(res.successes.nonEmpty)

  setStatesList(res.successes)
  def selectedResult = selectedState.success map graphOfResult
}


class SelectorResultPanel
(val selector : Component with Selector,
 val constraints : ConstraintsMaps,
 val autosolveResultPanel : AutosolveResultPanel )
  extends BorderPanel with GraphPanelResultPanel {

  def printingOptions: PrintingOptions = autosolveResultPanel.printingOptions
  val graphUtils: GraphUtils = autosolveResultPanel.graphUtils
  val publisher : Publisher = autosolveResultPanel
  val visibilitySet: VisibilitySet.T = autosolveResultPanel.visibilitySet

  add(selectedResultGraphPanel, Position.Center)
  add(selector, Position.South)

  def selectedResult = selector.selectedResult

  this listenTo selector
  reactions += {
    case PrintingOptionsUpdate =>
      add(selectedResultGraphPanel, Position.Center)

    case StateSelected(state) =>
      add(selectedResultGraphPanel, Position.Center)
      publish(Log(selector.selectedLog))

  }

}