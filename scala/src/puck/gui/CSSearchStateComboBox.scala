package puck.gui

import puck.graph.NodeKind
import puck.graph.constraints.search.ConstraintSolving

import scala.swing.{ComboBox, FlowPanel}
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
        val r = hd.internal.recording
        r()
        val value = (r.graph.coupling * 100).toInt
        val oldl = acc.getOrElse(value, List())
        aux(acc + (value -> (hd :: oldl)), tl)
    }

    aux(Map[Int, List[ConstraintSolving.FinalState[Kind]]](), l)
  }
}
class CSSearchStateComboBox[Kind <: NodeKind[Kind]](map : Map[Int, List[ConstraintSolving.FinalState[Kind]]])
  extends FlowPanel{

  val combobox2wrapper = new FlowPanel()
  var searchStateComboBox : ComboBox[ConstraintSolving.FinalState[Kind]] = _

  val couplingValues = new ComboBox(map.keys.toSeq)
  combobox2wrapper.contents += new ComboBox(map(couplingValues.selection.item))
  contents += couplingValues
  contents += combobox2wrapper

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
