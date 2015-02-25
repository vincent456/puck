package puck.gui.search

import puck.graph.constraints.search.ConstraintSolvingStateEvaluator
import puck.graph.io.VisibilitySet
import puck.graph.{Recording, ResultT}
import puck.search.{Search, SearchState}
import puck.util.{PuckLog, PuckLogger}

import scala.swing._

/**
 * Created by lorilan on 22/10/14.
 */
class ResultPanel
( initialRecord : Recording,
  res : Search[ResultT],
  logger : PuckLogger,
  printId : () => Boolean,
  printSig: () => Boolean,
  visibility : VisibilitySet)
      extends BoxPanel(Orientation.Vertical){

  implicit val defaultVerbosity : PuckLog.Verbosity = (PuckLog.NoSpecialContext, PuckLog.Info)

  type ST = SearchState[ResultT]

  val evaluator = new ConstraintSolvingStateEvaluator(initialRecord)


  logger.write("comparing final states : ")

  val sortedRes: Map[Int, Seq[ST]] =
    puck.util.Time.time(logger, defaultVerbosity){
      //evaluator.filterDifferentStates(evaluator.sort(res.initialState.iterator.toSeq))
      evaluator.filterDifferentStates(evaluator.sort(res.finalStates))
    }
  //val sortedRes  =  CSSearchStateComboBox.sort(res.finalStates)

  val total = sortedRes.foldLeft(0) { case (acc, (_, l)) => acc + l.size}

  logger.writeln("%d states explored".format(res.exploredStates))
  logger.writeln("%d final states".format(res.finalStates.size))
  logger.writeln("%d different final states ".format(total))

  this deafTo this

  reactions += {
    case e => publish(e)
  }

  if(sortedRes.nonEmpty) {
    //val comp : Component = new StateComparator(initialRecord, sortedRes, printId, printSig, visibility)
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
