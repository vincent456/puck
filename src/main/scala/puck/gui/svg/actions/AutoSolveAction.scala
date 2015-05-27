package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing. AbstractAction

import puck.graph.io.{Visible, VisibilitySet}
import puck.graph.{DependencyGraph, ResultT, ConcreteNode}
import puck.graph.constraints.search.{CSInitialSearchState, ConstraintSolvingSearchEngineBuilder}
import puck.graph.constraints.search.ConstraintSolvingSearchEngineBuilder.TryAllCSSEBuilder
import puck.gui.search.{StateSelected, SimpleStateSelector}
import puck.gui.svg.{SVGPanel, SVGController}
import puck.search.Search

import scala.swing.BorderPanel.Position
import scala.swing._
import scala.swing.Dialog.{Message, Options, Result}
import VisibilitySet._

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
    println("setGraph, visibilitySet :" + visibilitySet.toSeq.sorted)
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

  println("violation target " + violationTarget.id)
  import controller.{graph, graphUtils, dg2ast}, graphUtils._
  val initalNumNodes = graph.numNodes

  def nodeVisibles(graph : DependencyGraph) = {
    val users = graph.usersOf(violationTarget.id)
    val targetAndAncestors =
      graph.containerPath(violationTarget.id).toSet + violationTarget.id
    println("targetAndAncestors = " + targetAndAncestors.toSeq.sorted)
    val idSet = users.foldLeft(targetAndAncestors){
      (s,id) => (graph.containerPath(id).toSet + id) union s
    }
    //initialVisibleSet must be recomputed in case of moved nodes
    val initialVisibleSet =
      idSet.foldLeft(VisibilitySet.allHidden(graph))(_.setVisibility(_, Visible))

    println(initialVisibleSet.toSeq.sorted)
    println(initalNumNodes+ " "+ graph.numNodes)
    Seq.range(initalNumNodes, graph.numNodes).foldLeft(initialVisibleSet){
      (s,id) =>
        (graph.containerPath(id) :+ id).foldLeft(s)(_.setVisibility(_, Visible))
    }

  }

  val initialVisibleSet = nodeVisibles(graph)


  private def dialog(res : Search[ResultT]) : Result.Value = {
    val message = "Select type member to abstract"
    val title = "TypeDecl abstraction options"
    val panel = new SplitPane(Orientation.Vertical) {
      dividerSize = 3
      preferredSize = new Dimension(1024, 780)

      leftComponent = GraphScrollPane(controller,
        res.initialState.result,
        initialVisibleSet)

      val stateSelector = new SimpleStateSelector(res.allStatesByDepth)
      val rightDocWrapper = GraphScrollPane(controller,
        stateSelector.selectedState.result,
         nodeVisibles(stateSelector.selectedState.result))

      rightComponent = new BorderPanel{
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

    Dialog.showConfirmation(null, panel.peer, title, Options.OkCancel, Message.Plain)
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    val builder = new ConstraintSolvingSearchEngineBuilder(
        violationsKindPriority,
        transformationRules,
        TryAllCSSEBuilder,
        CSInitialSearchState.targetedInitialState(violationTarget))
    val engine = builder.apply(dg2ast.initialRecord, graph.mileStone, automaticConstraintLoosening = false)
    engine.explore()

    dialog(engine)


  }
}
