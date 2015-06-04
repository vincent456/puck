package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.{JLabel, JComponent, AbstractAction}

import puck.PuckError
import puck.graph.io.{Visible, VisibilitySet}
import puck.graph._
import puck.graph.constraints.search.{CSInitialSearchState, ConstraintSolvingSearchEngineBuilder}
import puck.graph.constraints.search.ConstraintSolvingSearchEngineBuilder.TryAllCSSEBuilder
import puck.gui.PuckConsolePanel
import puck.gui.search.{SortedElementSelector, SimpleElementSelector, StateSelected}
import puck.gui.svg.{SVGPanel, SVGController}
import puck.search.{ErrorState, SearchState, Search}
import puck.util.Logged

import scala.swing.BorderPanel.Position
import scala.swing._
import scala.swing.Dialog.{Message, Options, Result}
import VisibilitySet._

import scala.swing.event.Event


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

class AutoSolveAction
( violationTarget : ConcreteNode,
  controller : SVGController)
  extends AbstractAction("Solve (auto choices, choose result)") {

  import controller.{graph, graphUtils, dg2ast}, graphUtils._
  val initalNumNodes = graph.numNodes

  def ancestorsUsersAndUsersAncestorsOf(graph : DependencyGraph, id : NodeId) = {
    val users = graph.usersOf(id)
    val targetAndAncestors =
      graph.containerPath(id).toSet + violationTarget.id

    users.foldLeft(targetAndAncestors){
      (s,id) => (graph.containerPath(id).toSet + id) union s
    }

  }
  def nodeVisibles(graph : DependencyGraph) = {
    val idSet = ancestorsUsersAndUsersAncestorsOf(graph, violationTarget.id)

    val initialVisibleSet =
      idSet.foldLeft(VisibilitySet.allHidden(graph))(_.setVisibility(_, Visible))

    Seq.range(initalNumNodes, graph.numNodes).foldLeft(initialVisibleSet){
      (s,id) =>
        ancestorsUsersAndUsersAncestorsOf(graph, id)
          .foldLeft(s)(_.setVisibility(_, Visible))
    }

  }

  val initialVisibleSet = nodeVisibles(graph)

  case class ErrorSelected(state : ErrorState[ResultT]) extends Event

  class AutoSolveFailurePanel( res : Search[ResultT]) extends SplitPane(Orientation.Horizontal) {

    val failureSelector =
      new SortedElementSelector[ErrorState[ResultT]](res.failuresByDepth, ErrorSelected.apply)


    def selectedResult = failureSelector.selectedState

    val console = new PuckConsolePanel()
    val upPane =  new SplitPane(Orientation.Vertical) {

      dividerSize = 3
      preferredSize = new Dimension(1024, 780)

      leftComponent = GraphScrollPane(controller,
        res.initialState.loggedResult.value,
        initialVisibleSet)

      // val stateSelector = new SortedStateSelector(res.allStatesByDepth)

      val rightDocWrapper = GraphScrollPane(controller,
        selectedResult.prevState.loggedResult.value,
        nodeVisibles(selectedResult.prevState.loggedResult.value))

      rightComponent = new BorderPanel {
        add(rightDocWrapper, Position.Center)
        add(failureSelector, Position.South)
      }

      this listenTo failureSelector
      reactions += {
        case ErrorSelected(state) =>
          rightDocWrapper.setGraph(selectedResult.prevState.loggedResult.value,
            nodeVisibles(selectedResult.prevState.loggedResult.value))
          console.textArea.text =
            state.result.written +
            state.result.value.getMessage
      }
    }

    leftComponent = upPane
    rightComponent = console


  }

  class AutosolveResultPanel( res : Search[ResultT]) extends SplitPane(Orientation.Vertical) {

    val stateSelector = new SimpleElementSelector[SearchState[ResultT]](StateSelected.apply)
    stateSelector.setStatesList(res.successes)

    def selectedResult = stateSelector.selectedState.loggedResult

    dividerSize = 3
    preferredSize = new Dimension(1024, 780)

    leftComponent = GraphScrollPane(controller,
      res.initialState.loggedResult.value,
      initialVisibleSet)

    // val stateSelector = new SortedStateSelector(res.allStatesByDepth)

    val rightDocWrapper = GraphScrollPane(controller,
      selectedResult.value,
      nodeVisibles(stateSelector.selectedState.loggedResult.value))

    rightComponent = new BorderPanel {
      add(rightDocWrapper, Position.Center)
      add(stateSelector, Position.South)
    }

    this listenTo stateSelector
    reactions += {
      case StateSelected(state) =>
        rightDocWrapper.setGraph(selectedResult.value,
          nodeVisibles(selectedResult.value))
    }

  }


  private def dialog(res : Search[ResultT]) : Option[(Result.Value, Logged[ResultT])] = {
    val title = "Auto solve"

    val confirm : JComponent => Result.Value =
      c =>
        Dialog.showConfirmation(null, c, title, Options.OkCancel, Message.Plain)

    if(res.successes.isEmpty){
      confirm(new JLabel("No solution")) match{
        case Result.Ok =>
          val panel = new AutoSolveFailurePanel(res)
          val resVal = confirm(panel.peer)
          Some((resVal, panel.selectedResult.prevState.loggedResult))
        case Result.Cancel => None
      }
    }
    else {
      val panel = new AutosolveResultPanel(res)
      val resVal = confirm(panel.peer)
      Some((resVal, panel.selectedResult))
    }
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    val builder = new ConstraintSolvingSearchEngineBuilder(
        violationsKindPriority,
        transformationRules,
        TryAllCSSEBuilder,
        CSInitialSearchState.targetedInitialState(violationTarget))
    val engine = builder.apply(dg2ast.initialRecord, graph.mileStone, automaticConstraintLoosening = false)
    engine.explore()

    try {
      printErrOrPushGraph(controller, "Auto solve action : ") {
        dialog(engine) match {
          case None => LoggedError(new PuckError("no solution"))
          case Some((Result.Ok, g)) => g.toLoggedTry
          case _ => LoggedError(new PuckError("cancelled"))
        }
      }
    }
    catch{
     case t : Throwable =>
       println("catched "+ t.getMessage)
       t.printStackTrace()
    }

  }
}
