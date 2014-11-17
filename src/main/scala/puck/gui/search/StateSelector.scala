package puck.gui.search

import puck.graph.{ResultT, graphOfResult}
import puck.gui.{ApplyOnCodeRequest, GraphDisplayRequest, SearchStateSeqPrintingRequest}
import puck.search.SearchState

import scala.swing._
import scala.swing.event.{Event, SelectionChanged}

/**
 * Created by lorilan on 18/09/14.
 */
case class StateSelected(box : StateSelector) extends Event

class StateSelector
( map : Map[Int, Seq[SearchState[ResultT]]],
  printId : () => Boolean,
  printSig: () => Boolean)
  extends BoxPanel(Orientation.Vertical){

  type StateT = SearchState[ResultT]

  val firstLine = new FlowPanel()
  val combobox2wrapper = new FlowPanel()
  val couplingValues = new ComboBox(map.keys.toSeq)

  var searchStateComboBox : ComboBox[StateT] =
    new ComboBox(map(couplingValues.selection.item))
  combobox2wrapper.contents += searchStateComboBox

  firstLine.contents += couplingValues
  firstLine.contents += combobox2wrapper
  contents += firstLine

  val secondLine = new FlowPanel()
  secondLine.contents += new Button(""){
      action = new Action("Show"){
        def apply() {
          StateSelector.this publish
            GraphDisplayRequest(couplingValues.selection.item + " " + searchStateComboBox.selection.item.uuid(),
              graphOfResult(searchStateComboBox.selection.item.result), printId(), printSig())
        }
      }
    }

  secondLine.contents += new Button(""){
    action = new Action("History"){
      def apply() {

        val state: SearchState[ResultT] = searchStateComboBox.selection.item
        var id = -1

        StateSelector.this publish SearchStateSeqPrintingRequest(state.uuid()+"history",
          state.ancestors(includeSelf = true), Some({s => id +=1
            id.toString}), printId(), printSig())

      }
    }
  }


  secondLine.contents += new Button(""){
    action = new Action("Apply"){
      def apply(){
        StateSelector.this publish ApplyOnCodeRequest(searchStateComboBox.selection.item.result)
      }
    }
  }
  contents += secondLine
  this listenTo couplingValues.selection

  reactions += {
    case SelectionChanged(cb) if cb == couplingValues =>
      combobox2wrapper.contents.clear()
      searchStateComboBox = new ComboBox(map(couplingValues.selection.item))
      combobox2wrapper.contents += searchStateComboBox
      this listenTo searchStateComboBox.selection

    case SelectionChanged(cb) if cb == searchStateComboBox =>
      this.publish(StateSelected(this))
  }

  def selectedState = searchStateComboBox.selection.item

}
