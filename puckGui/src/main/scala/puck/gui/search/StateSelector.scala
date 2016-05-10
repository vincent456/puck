/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.gui.search

import puck.graph._
import puck.search.SearchState

import scala.swing._
import scala.swing.event.{Event, SelectionChanged}


case class StateSelected[T](state : SearchState[DecoratedGraph[T]]) extends Event

class SimpleElementSelector[T]
 ( evtGen : T => Event)
  extends FlowPanel {
  var searchStateComboBox : ComboBox[T] = _

  def publish() : Unit = {
    this.publish(evtGen(searchStateComboBox.selection.item))
  }

  def selectedState = searchStateComboBox.selection.item

  def setStatesList(states : Seq[T]): Unit ={
    if(searchStateComboBox != null) {
      contents.clear()
      this deafTo searchStateComboBox
    }
    searchStateComboBox = new ComboBox(states)
    this listenTo searchStateComboBox.selection
    contents += searchStateComboBox
    this.revalidate()
    publish()
  }

  reactions += {
    case SelectionChanged(cb) => publish()
  }


}

class SortedElementSelector[T]
(map : Map[Int, Seq[T]],
 evtGen : T => Event)
  extends BoxPanel(Orientation.Vertical) {
  val firstLine = new FlowPanel()
  val simpleStateSelector = new SimpleElementSelector[T](evtGen)
  val couplingValues = new ComboBox(map.keys.toSeq.sorted)

  simpleStateSelector.setStatesList(map(couplingValues.selection.item))

  firstLine.contents += couplingValues
  firstLine.contents += simpleStateSelector
  contents += firstLine

  this listenTo couplingValues.selection
  this listenTo simpleStateSelector
  this deafTo this

  reactions += {
    case SelectionChanged(cb) =>
      simpleStateSelector.setStatesList(map(couplingValues.selection.item))
    case evt  => publish(evt)
  }

  def selectedState = simpleStateSelector.selectedState

}
