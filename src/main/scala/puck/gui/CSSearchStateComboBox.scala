package puck.gui

import puck.graph.{ResultT, graphOfResult, NodeKind}
import puck.search.SearchState

import scala.swing.{Action, Button, ComboBox, FlowPanel}
import scala.swing.event.{SelectionChanged, Event}

/**
 * Created by lorilan on 18/09/14.
 */
case class StateSelected(box : CSSearchStateComboBox) extends Event

object CSSearchStateComboBox{

  def sort(l : Seq[SearchState[ResultT]])={
    def aux( acc : Map[Int, Seq[SearchState[ResultT]]],
               l : Seq[SearchState[ResultT]]) : Map[Int, Seq[SearchState[ResultT]]] =
      if(l.isEmpty) acc
      else{
        val graph = graphOfResult(l.head.result)

        val value = (graph.coupling * 100).toInt
        val olds = acc.getOrElse(value, Seq())
        aux(acc + (value -> (l.head +: olds)), l.tail)
      }
    aux(Map[Int, Seq[SearchState[ResultT]]](), l)
  }
}
class CSSearchStateComboBox(map : Map[Int, Seq[SearchState[ResultT]]])
  extends FlowPanel{

  type StateT = SearchState[ResultT]

  val combobox2wrapper = new FlowPanel()
  val couplingValues = new ComboBox(map.keys.toSeq)

  var searchStateComboBox : ComboBox[StateT] =
    new ComboBox(map(couplingValues.selection.item))
  combobox2wrapper.contents += searchStateComboBox

  contents += couplingValues
  contents += combobox2wrapper
  val showButton =
    contents += new Button(""){
      action = new Action("Show"){
        def apply() {
          CSSearchStateComboBox.this publish
            GraphDisplayRequest(couplingValues.selection.item + " " + searchStateComboBox.selection.item.uuid(),
              graphOfResult(searchStateComboBox.selection.item.result))
        }
      }
    }

  contents += new Button(""){
    action = new Action("History"){
      def apply() {

        val state: SearchState[ResultT] = searchStateComboBox.selection.item
        var id = -1

        CSSearchStateComboBox.this publish SearchStateSeqPrintingRequest(state.uuid()+"history",
          state.ancestors(includeSelf = true), Some({s => id +=1
            id.toString}))

      }
    }
  }


  contents += new Button(""){
    action = new Action("Apply"){
      def apply(){
        CSSearchStateComboBox.this publish ApplyOnCodeRequest(searchStateComboBox.selection.item.result)
      }
    }
  }

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
