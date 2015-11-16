package puck.gui.svg.actions

import java.awt.Dimension

import puck.graph._
import puck.graph.io.{Visible, VisibilitySet}
import VisibilitySet._
import puck.gui.PuckConsolePanel
import puck.gui.search.{StateSelected, SimpleElementSelector, SortedElementSelector}
import puck.gui.svg.{PUCKSVGCanvas, SVGController}
import puck.search.{SearchState, Search}
import puck.util._

import scala.swing.BorderPanel.Position
import scala.swing.TabbedPane.Page
import scala.swing.event.Event
import scala.swing._

import scalaz.syntax.writer._

object GraphScrollPane {
  def apply
  ( controller : SVGController,
    graph: DependencyGraph,
    visibilitySet: VisibilitySet.T) : GraphScrollPane = {
    val gsp = new GraphScrollPane(controller)
    gsp.setGraph(graph, visibilitySet)
    gsp
  }
}
class GraphScrollPane(controller : SVGController) extends ScrollPane(){

  import controller.executor

  def setGraph(graph: DependencyGraph, visibilitySet: VisibilitySet.T): Unit ={
    //println("setGraph, visibilitySet :" + visibilitySet.toSeq.sorted)
    import controller.swingInvokeLater
    val doc =
      SVGController.documentFromGraph(graph,
        controller.graphUtils,
        controller.printingOptions.
          copy(visibility =visibilitySet))(
          SVGController.documentFromGraphErrorMsgGen(msg =>
            swingInvokeLater( () => controller.console.appendText(msg)))){
        case d =>
          val c = new PUCKSVGCanvas(PUCKSVGCanvas.deafListener)
          swingInvokeLater { () =>
            c.setDocument(d)
            viewportView = Component.wrap(c)
          }
      }
  }
}


case class ErrorSelected(state : SearchState[SResult]) extends Event
case class Log(msg : String) extends Event


class AutosolveResultPanel
( violationTarget : ConcreteNode,
  controller : SVGController,
  res : Search[SResult]) extends SplitPane(Orientation.Horizontal) {

  import controller.graph

  def ancestorsUsersAndUsersAncestorsOf(graph : DependencyGraph, id : NodeId) = {
    val users = graph.usersOf(id)
    val targetAndAncestors =
      graph.containerPath(id).toSet + violationTarget.id

    users.foldLeft(targetAndAncestors){
      (s,id) => (graph.containerPath(id).toSet + id) union s
    }

  }
  val initialNumNodes = graph.numNodes
  def nodeVisibles(graph : DependencyGraph) : VisibilitySet.T = {
    val idSet = ancestorsUsersAndUsersAncestorsOf(graph, violationTarget.id)

    val initialVisibleSet =
      idSet.foldLeft(VisibilitySet.allHidden(graph))(_.setVisibility(_, Visible))

    Seq.range(initialNumNodes, graph.numNodes).foldLeft(initialVisibleSet){
      (s,id) =>
        ancestorsUsersAndUsersAncestorsOf(graph, id)
          .foldLeft(s)(_.setVisibility(_, Visible))
    }

  }


  val initialVisibleSet = nodeVisibles(graph)

  def selectedResult : Logged[DependencyGraph] = activePanel.selectedResult

  val successesTab = 0
  val failuresTab = 1


  val successesPanel =
    if(res.successes.nonEmpty) new SuccessPanel(controller,res, nodeVisibles)
    else new DummyResultPanel(controller)

  val failurePanel =
    if(res.failures.nonEmpty) new FailurePanel(controller,res, nodeVisibles)
    else new DummyResultPanel(controller)

  this listenTo successesPanel
  this listenTo failurePanel

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

    leftComponent = GraphScrollPane(controller, graph, initialVisibleSet)

    // val stateSelector = new SortedStateSelector(res.allStatesByDepth)
    rightComponent = tabs

  }

  leftComponent = upPane
  rightComponent = console

  reactions += {
    case Log(msg) => console.textArea.text = msg
  }

}


trait ResultPanel {
  val controller : SVGController
  def selectedResult : Logged[DependencyGraph]

  def selectedResultVisibility = {
    val newNodes = Range(controller.graph.numNodes, selectedResult.value.numNodes)
    controller.printingOptions.visibility.setVisibility(newNodes, Visible)
  }

}

class DummyResultPanel(val controller : SVGController) extends FlowPanel with ResultPanel {
  def selectedResult : Logged[DependencyGraph] = controller.graph.set("")
}

class FailurePanel
( val controller : SVGController,
  res : Search[SResult],
  nodeVisibles : DependencyGraph => VisibilitySet.T
  ) extends BorderPanel with ResultPanel {

  assert(res.failures.nonEmpty)

  val failureSelector =
    new SortedElementSelector[SearchState[SResult]](res.failuresByDepth, ErrorSelected.apply)

  def selectedResult = failureSelector.selectedState.prevState.get.success map graphOfResult



  val rightDocWrapper = GraphScrollPane(controller,
    selectedResult.value,
    selectedResultVisibility)
    //nodeVisibles(selectedResult.value))

    add(rightDocWrapper, Position.Center)
    add(failureSelector, Position.South)

  this listenTo failureSelector
  reactions += {
    case ErrorSelected(state) =>
      rightDocWrapper.setGraph(selectedResult.value,
        selectedResultVisibility)
      publish(Log(state.fail.written +
        state.fail.value.getMessage))

  }
}

class SuccessPanel
( val controller : SVGController,
  res : Search[SResult],
  nodeVisibles : DependencyGraph => VisibilitySet.T
  ) extends BorderPanel with ResultPanel {

  assert(res.successes.nonEmpty)
  val lightKind = graphOfResult(res.successes.head.loggedResult.value).nodeKindKnowledge.lightKind
//  val stateSelector =
//    new SortedElementSelector(
//      res.successes.groupBy(st => (Metrics.weight(st.loggedResult.value, lightKind) * 100).toInt),
//      StateSelected.apply)

  val stateSelector = new SimpleElementSelector[SearchState[SResult]](StateSelected.apply)
  stateSelector.setStatesList(res.successes)

  def selectedResult = stateSelector.selectedState.success map graphOfResult

  val graphWrapper = GraphScrollPane(controller,
    selectedResult.value,
    selectedResultVisibility)
    //nodeVisibles(stateSelector.selectedState.loggedResult.value))

  add(graphWrapper, Position.Center)
  add(stateSelector, Position.South)
  this listenTo stateSelector
  reactions += {
    case StateSelected(state) =>
      graphWrapper.setGraph(selectedResult.value,
        selectedResultVisibility)
        //nodeVisibles(selectedResult.value))

      publish(Log(selectedResult.written))
  }

}