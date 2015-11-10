package puck.gui.search

import puck.graph.{SResult, DependencyGraph}
import puck.graph.io.VisibilitySet
import puck.graph.transformations.Transformation
import puck.searchNew.{Search, SearchState,ConstraintSolvingStateEvaluator}
import puck.util.{PuckLog, PuckLogger}

import scala.swing._

class ResultPanel
( initialRecord : Seq[Transformation],
  res : Search[SResult],
  logger : PuckLogger,
  printId : () => Boolean,
  printSig: () => Boolean,
  visibility : VisibilitySet.T)
      extends BoxPanel(Orientation.Vertical){

  implicit val defaultVerbosity : PuckLog.Verbosity = (PuckLog.NoSpecialContext, PuckLog.Info)

  type ST = SearchState[DependencyGraph]

  val evaluator = new ConstraintSolvingStateEvaluator(initialRecord)


  logger.write("comparing final states : ")

  val allStates = res.allStatesByDepth


//  val sortedRes: Map[Int, Seq[ST]] =
//    puck.util.Time.time(logger, defaultVerbosity){
//      evaluator.sortedDifferentStates(res.finalStates)
//    }
//  val total = sortedRes.foldLeft(0) { case (acc, (_, l)) => acc + l.size}


  val sortedRes = Map(-1 -> res.successes)
  val total = res.successes.size

  logger.writeln("%d states explored".format(res.exploredStates))
  logger.writeln("%d final states".format(res.successes.size))
  logger.writeln("%d different final states ".format(total))

  this deafTo this

  reactions += {
    case e => publish(e)
  }

  if(sortedRes.nonEmpty) {
    //val comp : Component = new StateComparator(initialRecord, sortedRes, printId, printSig, visibility)
    val comp0 : Component = new StateSelector(allStates, printId, printSig, visibility)
    contents += comp0

    this listenTo comp0


    val comp : Component = new StateSelector(sortedRes, printId, printSig, visibility)
    contents += comp

    this listenTo comp


    /*contents += new FlowPanel() {
      val couplingValues = new ComboBox(sortedRes.keys.toSeq)
      contents += couplingValues
      contents += Button("Print") {
        SearchResultPanel.this.
          publish(SearchStateSeqPrintingRequest(couplingValues.selection.item.toString,
          sortedRes(couplingValues.selection.item), None, printId(), printSig()))
      }
    }

    contents += Button("Print all") {
      SearchResultPanel.this.
        publish(SearchStateMapPrintingRequest(sortedRes, printId(), printSig()))
    }*/
  }

}
