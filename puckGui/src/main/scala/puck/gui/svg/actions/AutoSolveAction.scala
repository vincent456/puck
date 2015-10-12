package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.{JComponent, AbstractAction}

import puck.graph.constraints.search.{CSInitialSearchState, ConstraintSolvingSearchEngineBuilder}
import puck.graph._
import puck.gui.svg.SVGController
import puck.search.{TryAllSearchStrategy, Search}
import puck.util.Logged

import scala.swing._
import scala.swing.Dialog.{Message, Options, Result}




class AutoSolveAction
( violationTarget : ConcreteNode,
  controller : SVGController)
  extends AbstractAction("Solve (auto choices, choose result)") {

  private def dialog(res : Search[ResultT]) : Option[Logged[ResultT]] = {
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
        case Result.Ok =>  Some( panel.selectedResult)
        case Result.Cancel
          | Result.Closed => None
    }
  }

  import controller.{graph, graphUtils, dg2ast}, graphUtils._

  override def actionPerformed(e: ActionEvent): Unit = {
    val builder = new ConstraintSolvingSearchEngineBuilder(
        violationsKindPriority,
        transformationRules,
        new TryAllSearchStrategy,
        CSInitialSearchState.targetedInitialState(violationTarget))
    val engine = builder.apply(dg2ast.initialRecord, graph.mileStone, automaticConstraintLoosening = false)
    engine.explore()

    try {
      printErrOrPushGraph(controller, "Auto solve action : ") {
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
