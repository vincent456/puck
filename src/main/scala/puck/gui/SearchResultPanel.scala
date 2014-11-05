package puck.gui

import puck.graph.immutable.transformations.{Transformation, RecordingComparator}
import puck.graph.{ResultT, AccessGraph, graphOfResult, NodeKind}
import puck.search.{SearchState, Search}
import puck.util.{PuckLog, PuckLogger}

import scala.swing._

/**
 * Created by lorilan on 22/10/14.
 */
class SearchResultPanel[Kind <: NodeKind[Kind], T](initialRecord : Seq[Transformation[Kind, T]],
                                                   res : Search[ResultT[Kind, T]],
                                                   logger : PuckLogger)
      extends BoxPanel(Orientation.Vertical){

  implicit val defaultVerbosity : PuckLog.Verbosity = (PuckLog.NoSpecialContext, PuckLog.Info)

  type ST = SearchState[ResultT[Kind, T]]

  def filterDifferentStates(l : Seq[ST]): Seq[ST] = {
    def aux(l : Seq[ST], acc : Seq[ST]) : Seq[ST] = {
      if (l.nonEmpty) {
        aux(l.tail,
          if (!l.tail.exists { st =>
            AccessGraph.areEquivalent(initialRecord, graphOfResult(st.result),graphOfResult(l.head.result))
          })
            l.head +: acc
          else acc)
      }
      else acc
    }
    aux(l, Seq[ST]())
  }

  logger.write("comparing final states : ")

  val sortedRes: Map[Int, Seq[ST]] =
    puck.util.Time.time(logger, defaultVerbosity){

      //CSSearchStateComboBox.sort(res).mapValues(filterDifferentStates)
      // do not actually apply the function and hence give a false compute time
      CSSearchStateComboBox.sort[Kind, T](res.finalStates).foldLeft(Map[Int, Seq[ST]]()){
        case (acc, (k, v)) => acc + (k -> filterDifferentStates(v))
      }
    }
  //val sortedRes  =  CSSearchStateComboBox.sort(res)


  val total = sortedRes.foldLeft(0) { case (acc, (_, l)) => acc + l.size}

  logger.writeln("%d states explored".format(res.exploredStates))
  logger.writeln("%d final states".format(res.finalStates.size))
  logger.writeln("%d different final states ".format(total))

  this deafTo this

  reactions += {
    case e => //println("SearchResultPanel : chaining event")
      publish(e)
  }

  if(sortedRes.nonEmpty) {
    //val comp : Component = new CSSearchStateComparator(sortedRes)
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
