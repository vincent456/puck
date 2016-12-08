package puck.gui

import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.constraints.search._
import puck.gui.svg.actions.AutoSolveAction
import puck.search._

import scala.swing.Dialog.{Message, Options, Result}
import scala.swing._

/**
  * Created by Loïc Girault on 12/8/16.
  */

object SearchDialog{

  def apply(control : PuckControl) : Unit =
    control.constraints foreach {
      cm =>
        val sd = new SearchDialog(control)
        Dialog.showConfirmation(
          message = sd.peer,
          title = "Search Settings",
          optionType = Options.OkCancel,
          messageType = Message.Plain
        ) match {
          case Result.Ok =>
            val s = sd.strategyCB.selection.item()
            val vns = sd.vnStrategyCB.selection.item
            val c = sd.controlCB.selection.item(control.graph.newGraph(mutabilitySet = control.mutabilitySet), cm, vns)
            Swing onEDT new AutoSolveAction(control.Bus, cm, control.printingOptionsControl,
              s, c)(control.graphUtils, control.nodeKindIcons).apply()
          case _ => ()
        }
    }
}

class SearchDialog(control : PuckControl) extends BoxPanel(Orientation.Vertical) {

  type ControlBuilder = (DependencyGraph, ConstraintsMaps, VirtualNodePolicy) => SearchControl[DecoratedGraph[Any]]
  type StrategyBuilder = () => SearchStrategy[DecoratedGraph[Any]]


  import control.graphUtils
  val controlCB = new ComboBox[ControlBuilder](List(
    new ControlBuilder {
      override val toString = "Control with Heuristic"
      def apply(dg : DependencyGraph, cm : ConstraintsMaps, virtualNodePolicicy : VirtualNodePolicy) =
        new ControlWithHeuristic(graphUtils.Rules, dg, cm, virtualNodePolicicy,
          graphUtils.violationsKindPriority).
          asInstanceOf[SearchControl[DecoratedGraph[Any]]]
    },
    new ControlBuilder {
      override val toString = "Blind control"
      def apply(dg : DependencyGraph, cm : ConstraintsMaps, virtualNodePolicicy : VirtualNodePolicy) =
        new BlindControl(graphUtils.Rules, dg, cm,
          virtualNodePolicicy, graphUtils.violationsKindPriority).
          asInstanceOf[SearchControl[DecoratedGraph[Any]]]
    }
  ))



  val strategyCB =  new ComboBox(List[StrategyBuilder](
    new (() => SearchStrategy[DecoratedGraph[Any]]) {
      override val toString = "Depth-first Strategy"
      def apply() = new DepthFirstSearchStrategy()
    },
    new (() => SearchStrategy[DecoratedGraph[Any]]) {
      override val toString = "A* Strategy"
      def apply() = {
        val f = control.chooseMetric
        new AStarSearchStrategy(DecoratedGraphEvaluator.equalityByMapping(x => f(x).toDouble))
      }
    },
    new (() => SearchStrategy[DecoratedGraph[Any]]) {
      override val toString = "Breadth-first Strategy"
      def apply() = new BreadthFirstSearchStrategy()
    }
  ))

  val vnStrategyCB =  new ComboBox(List[VirtualNodePolicy](NoVirtualNodes, WithVirtualNodes))

  contents += controlCB
  contents += strategyCB
  contents += vnStrategyCB

}
