package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.{JComponent, AbstractAction}

import puck.graph._
import puck.graph.constraints.search.CouplingConstraintSolvingControl
import puck.gui.svg.SVGController
import puck.search.{Search, SearchEngine, DepthFirstSearchStrategy}
import puck.util.Logged

import scala.swing._
import scala.swing.Dialog.{Message, Options, Result}




class AutoSolveAction
( violationTarget : ConcreteNode,
  controller : SVGController)
  extends AbstractAction("Solve (auto choices, choose result)") {

  private def dialog(res : Search[SResult]) : Option[Logged[DependencyGraph]] = {
    val title = "Auto solve"

    val confirm : JComponent => Result.Value =
      c =>
        Dialog.showConfirmation(null, c, title, Options.OkCancel, Message.Plain)

//    if(res.successes.isEmpty){
//      confirm(new JLabel("No solution")) match{
//        case Result.Ok =>
//          val panel = new AutosolveResultPanel(violationTarget, controller, res)
//          val resVal = confirm(panel.peer)
//          Some((resVal, panel.selectedResult))
//        case Result.Cancel => None
//      }
//    }
//    else {
    val panel = new AutosolveResultPanel(violationTarget, controller, res)
    confirm(panel.peer) match {
        case Result.Ok => Some( panel.selectedResult)
        case Result.Cancel
           | Result.Closed => None
    }
  }

  import controller.graph

  override def actionPerformed(e: ActionEvent): Unit = {
    val searchControlStrategy =
      new CouplingConstraintSolvingControl(
        new DepthFirstSearchStrategy[(DependencyGraph, Int)],
        controller.graphUtils.transformationRules,
        graph.mileStone, violationTarget)

    val engine =
      new SearchEngine(searchControlStrategy.initialState,
        searchControlStrategy)

    engine.explore()

    try {
      puck.actions.printErrOrPushGraph(controller, "Auto solve action : ") {
        dialog(engine) match {
          case Some(g) => g.toLoggedTry
          case None => LoggedError("cancelled")
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
