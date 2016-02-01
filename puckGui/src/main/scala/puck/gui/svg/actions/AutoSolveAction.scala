package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.{JComponent, AbstractAction}

import puck.graph._
import puck.graph.constraints.search.{GraphConstraintSolvingStateEvaluator, CouplingConstraintSolvingControl}
import puck.graph.io.PrintingOptions
import puck.gui.PrintingOptionsControl
import puck.gui.svg.SVGController
import puck.search.{Search, SearchEngine, DepthFirstSearchStrategy}
import puck.util.Logged

import scala.swing._
import scala.swing.Dialog.{Message, Options, Result}




class AutoSolveAction
( publisher : Publisher,
  violationTarget : ConcreteNode,
  printingOptionsControl: PrintingOptionsControl)
(implicit graph : DependencyGraph,
  graphUtils: GraphUtils)
  extends AbstractAction("Solve [BETA - under development]") {

  private def dialog(res : Search[SResult]) : Option[Logged[DependencyGraph]] = {
    val title = "Solve"

    val confirm : Component => Result.Value =
      c =>
        Dialog.showConfirmation(null, c.peer, title, Options.OkCancel, Message.Plain)

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
    val panel = new AutosolveResultPanel(publisher, violationTarget, printingOptionsControl, res)
    confirm(panel) match {
        case Result.Ok => Some( panel.selectedResult)
        case Result.Cancel
           | Result.Closed => None
    }
  }


  override def actionPerformed(e: ActionEvent): Unit = {
    //val g = graph.mileStone
    val g = graph.nodes.foldLeft(graph.mileStone){
      case (g, n) => n.kind.kindType match {
        case TypeDecl => g.setMutability(n.id, false)
        case _ => g
      }
    }

    val searchControlStrategy =
      new CouplingConstraintSolvingControl(
        graphUtils.transformationRules,
        g, violationTarget)

    val engine =
      new SearchEngine(
        new DepthFirstSearchStrategy[(DependencyGraph, Int)],
        searchControlStrategy/*,
        evaluator = Some(GraphConstraintSolvingStateEvaluator)*/)

    engine.explore()

    try {
      puck.actions.printErrOrPushGraph(publisher, "Solve action : ") {
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
