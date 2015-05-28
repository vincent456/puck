package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.{JLabel, JComponent, AbstractAction}

import puck.PuckError
import puck.graph.io.{Visible, VisibilitySet}
import puck.graph.{NodeId, DependencyGraph, ResultT, ConcreteNode}
import puck.graph.constraints.search.{CSInitialSearchState, ConstraintSolvingSearchEngineBuilder}
import puck.graph.constraints.search.ConstraintSolvingSearchEngineBuilder.TryAllCSSEBuilder
import puck.gui.search.{SimpleStateSelector, StateSelected, SortedStateSelector}
import puck.gui.svg.{SVGPanel, SVGController}
import puck.search.Search

import scala.swing.BorderPanel.Position
import scala.swing._
import scala.swing.Dialog.{Message, Options, Result}
import VisibilitySet._

import scalaz.{\/-, -\/}

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


  private def dialog(res : Search[ResultT]) : Option[(Result.Value, ResultT)] = {
    val title = "Auto solve"

    val confirm : JComponent => Result.Value =
      c =>
      Dialog.showConfirmation(null, c, title, Options.OkCancel, Message.Plain)
      if(res.finalStates.isEmpty){
        val _ = confirm(new JLabel("No solution"))
        None
      }
      else {
        val stateSelector = new SimpleStateSelector
        stateSelector.setStatesList(res.finalStates)

        val panel = new SplitPane(Orientation.Vertical) {
          dividerSize = 3
          preferredSize = new Dimension(1024, 780)

          leftComponent = GraphScrollPane(controller,
            res.initialState.result,
            initialVisibleSet)

          // val stateSelector = new SortedStateSelector(res.allStatesByDepth)

          val rightDocWrapper = GraphScrollPane(controller,
            stateSelector.selectedState.result,
            nodeVisibles(stateSelector.selectedState.result))

          rightComponent = new BorderPanel {
            add(rightDocWrapper, Position.Center)
            add(stateSelector, Position.South)
          }

          this listenTo stateSelector
          reactions += {
            case StateSelected(state) =>
              rightDocWrapper.setGraph(stateSelector.selectedState.result,
                nodeVisibles(stateSelector.selectedState.result))
          }

        }
        val resVal =confirm(panel.peer)
        Some((resVal, stateSelector.selectedState.result))
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

    printErrOrPushGraph(controller, "Auto solve action : ") {
      dialog(engine) match {
        case None => -\/(new PuckError("no solution"))
        case Some((Result.Ok, g)) => \/-(g)
        case _ => -\/(new PuckError("cancelled"))
      }
    }

  }
}
