package puck.gui

import puck.graph.mutable.backTrack.Recording
import puck.graph.mutable.NodeKind
import puck.graph.mutable.constraints.search.ConstraintSolving
import puck.graph.mutable.constraints.search.ConstraintSolving.FinalState
import puck.search.SearchState

import scala.swing.{Action, Button, ComboBox, FlowPanel}
import scala.swing.event.{SelectionChanged, Event}

/**
 * Created by lorilan on 18/09/14.
 */
case class StateSelected[Kind <: NodeKind[Kind]](box : CSSearchStateComboBox[Kind]) extends Event

object CSSearchStateComboBox{
  def sort[Kind <: NodeKind[Kind]](l : Seq[ConstraintSolving.FinalState[Kind]])={
    def aux(acc : Map[Int, Seq[ConstraintSolving.FinalState[Kind]]], l : Seq[ConstraintSolving.FinalState[Kind]]) :
    Map[Int, Seq[ConstraintSolving.FinalState[Kind]]] =
      if(l.isEmpty) acc
      else{
        val recording = l.head.result
        recording()
        val value = (recording.graph.coupling * 100).toInt
        val olds = acc.getOrElse(value, Seq())
        aux(acc + (value -> (l.head +: olds)), l.tail)
      }
    aux(Map[Int, Seq[ConstraintSolving.FinalState[Kind]]](), l)
  }
}
class CSSearchStateComboBox[Kind <: NodeKind[Kind]](map : Map[Int, Seq[ConstraintSolving.FinalState[Kind]]])
  extends FlowPanel{

  val combobox2wrapper = new FlowPanel()
  val couplingValues = new ComboBox(map.keys.toSeq)

  var searchStateComboBox : ComboBox[ConstraintSolving.FinalState[Kind]] =
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
              Some(searchStateComboBox.selection.item.result))
        }
      }
    }

  contents += new Button(""){
    action = new Action("History"){
      def apply() {

        val state: SearchState[Recording[Kind], _] = searchStateComboBox.selection.item
        var id = -1

        CSSearchStateComboBox.this publish SearchStateSeqPrintingRequest[Kind](state.uuid()+"history",
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
