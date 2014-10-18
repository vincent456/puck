package puck.gui

import puck.graph.NodeKind
import puck.graph.backTrack.Recording
import puck.graph.constraints.search.ConstraintSolving
import puck.graph.constraints.search.ConstraintSolving.FinalState
import puck.search.SearchState

import scala.swing.{Action, Button, ComboBox, FlowPanel}
import scala.swing.event.{SelectionChanged, Event}

/**
 * Created by lorilan on 18/09/14.
 */
case class StateSelected[Kind <: NodeKind[Kind]](box : CSSearchStateComboBox[Kind]) extends Event

object CSSearchStateComboBox{
  def sort[Kind <: NodeKind[Kind]](l : List[ConstraintSolving.FinalState[Kind]])={
    def aux(acc : Map[Int, List[ConstraintSolving.FinalState[Kind]]], l : List[ConstraintSolving.FinalState[Kind]]) :
    Map[Int, List[ConstraintSolving.FinalState[Kind]]] = l match  {
      case List() => acc
      case hd :: tl =>
        val recording = hd.result
        recording()
        val value = (recording.graph.coupling * 100).toInt
        val oldl = acc.getOrElse(value, List())
        aux(acc + (value -> (hd :: oldl)), tl)
    }

    aux(Map[Int, List[ConstraintSolving.FinalState[Kind]]](), l)
  }
}
class CSSearchStateComboBox[Kind <: NodeKind[Kind]](map : Map[Int, List[ConstraintSolving.FinalState[Kind]]])
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

        CSSearchStateComboBox.this publish SearchStateListPrintingRequest[Kind](state.uuid()+"history",
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
