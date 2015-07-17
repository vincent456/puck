package puck.gui.svg.actions

import java.awt.Dimension

import puck.graph._
import puck.graph.io.{Visible, VisibilitySet}
import VisibilitySet._
import puck.gui.PuckConsolePanel
import puck.gui.search.{StateSelected, SimpleElementSelector, SortedElementSelector}
import puck.gui.svg.{SVGPanel, SVGController}
import puck.search.{SearchState, ErrorState, Search}
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
    visibilitySet: VisibilitySet.T) : GraphScrollPane ={
    val gsp = new GraphScrollPane(controller)
    gsp.setGraph(graph, visibilitySet)
    gsp
  }
}
class GraphScrollPane(controller : SVGController) extends ScrollPane(){
  def setGraph(graph: DependencyGraph, visibilitySet: VisibilitySet.T): Unit ={
    //println("setGraph, visibilitySet :" + visibilitySet.toSeq.sorted)
    val doc =
      SVGController.documentFromGraph(graph,
        controller.filesHandler,
        controller.printingOptions.
          copy(visibility =visibilitySet))(
          controller.console.appendText){
        case d => viewportView = Component.wrap(new SVGPanel(d, SVGPanel.deafListener))
      }
  }
}


case class ErrorSelected(state : ErrorState[ResultT]) extends Event
case class Log(msg : String) extends Event


class AutosolveResultPanel
( violationTarget : ConcreteNode,
  controller : SVGController,
  res : Search[ResultT]) extends SplitPane(Orientation.Horizontal) {

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

  def selectedResult : Logged[ResultT] = activePanel.selectedResult

  val successesTab = 0
  val failuresTab = 1


  val successesPanel =
    if(res.successes.nonEmpty) new SuccessPanel(controller,res, nodeVisibles)
    else new DummyResultPanel(controller.graph)

  val failurePanel =
    if(res.failures.nonEmpty) new FailurePanel(controller,res, nodeVisibles)
    else new DummyResultPanel(controller.graph)

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
   def selectedResult : Logged[ResultT]
}

class DummyResultPanel(g : DependencyGraph) extends FlowPanel with ResultPanel {
  def selectedResult : Logged[DependencyGraph] = g.set("")
}

class FailurePanel
( controller : SVGController,
  res : Search[ResultT],
  nodeVisibles : DependencyGraph => VisibilitySet.T
  ) extends BorderPanel with ResultPanel {

  assert(res.failures.nonEmpty)

  val failureSelector =
    new SortedElementSelector[ErrorState[ResultT]](res.failuresByDepth, ErrorSelected.apply)

  def selectedResult = failureSelector.selectedState.prevState.loggedResult


  val rightDocWrapper = GraphScrollPane(controller,
    selectedResult.value,
    controller.printingOptions.visibility)
    //nodeVisibles(selectedResult.value))

    add(rightDocWrapper, Position.Center)
    add(failureSelector, Position.South)

  this listenTo failureSelector
  reactions += {
    case ErrorSelected(state) =>
      rightDocWrapper.setGraph(selectedResult.value,
        nodeVisibles(selectedResult.value))
      publish(Log(state.result.written +
        state.result.value.getMessage))

  }
}

class SuccessPanel
( controller : SVGController,
  res : Search[ResultT],
  nodeVisibles : DependencyGraph => VisibilitySet.T
  ) extends BorderPanel with ResultPanel {

  assert(res.successes.nonEmpty)
  val lightKind = res.successes.head.loggedResult.value.nodeKindKnowledge.lightKind
  val stateSelector =
    new SortedElementSelector(
      res.successes.groupBy(st => (Metrics.weight(st.loggedResult.value, lightKind) * 100).toInt),
      StateSelected.apply)

//  val stateSelector = new SimpleElementSelector[SearchState[ResultT]](StateSelected.apply)
//  stateSelector.setStatesList(res.successes)



  def selectedResult = stateSelector.selectedState.loggedResult

  val graphWrapper = GraphScrollPane(controller,
    selectedResult.value,
    controller.printingOptions.visibility)
    //nodeVisibles(stateSelector.selectedState.loggedResult.value))

  add(graphWrapper, Position.Center)
  add(stateSelector, Position.South)
  this listenTo stateSelector
  reactions += {
    case StateSelected(state) =>
      graphWrapper.setGraph(selectedResult.value,
        controller.printingOptions.visibility)
        //nodeVisibles(selectedResult.value))

      publish(Log(selectedResult.written))
  }

}