package puck.gui

import puck.graph.constraints.search.ConstraintSolvingStateEvaluator
import puck.graph.immutable.transformations.{Transformation}
import puck.graph.{Recording, ResultT, AccessGraph, graphOfResult}
import puck.search.{SearchState, Search}
import puck.util.{PuckLog, PuckLogger}

import scala.swing._

/**
 * Created by lorilan on 22/10/14.
 */
class SearchResultPanel(initialRecord : Recording,
                        res : Search[ResultT],
                        logger : PuckLogger)
      extends BoxPanel(Orientation.Vertical){

  implicit val defaultVerbosity : PuckLog.Verbosity = (PuckLog.NoSpecialContext, PuckLog.Info)

  type ST = SearchState[ResultT]

  val evaluator = new ConstraintSolvingStateEvaluator(initialRecord)


  logger.write("comparing final states : ")

  val sortedRes: Map[Int, Seq[ST]] =
    puck.util.Time.time(logger, defaultVerbosity){
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
    //val comp : Component = new CSSearchStateComparator(initialRecord, sortedRes)
    val comp : Component = new CSSearchStateComboBox(sortedRes)
    contents += comp

    this listenTo comp


    contents += new FlowPanel() {
      val couplingValues = new ComboBox(sortedRes.keys.toSeq)
      contents += couplingValues
      contents += Button("Print") {
        SearchResultPanel.this.
          publish(SearchStateSeqPrintingRequest(couplingValues.selection.item.toString,
          sortedRes(couplingValues.selection.item), None))
      }
    }

    contents += Button("Print all") {
      SearchResultPanel.this.
        publish(SearchStateMapPrintingRequest(sortedRes))
    }
  }

  val allStates = res.initialState.iterator.toSeq.groupBy{ s => s.depth }
  val comp2 : Component = new CSSearchStateComboBox(allStates)
  contents += comp2

  this listenTo comp2

}
