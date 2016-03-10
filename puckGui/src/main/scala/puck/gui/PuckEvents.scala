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

package puck
package gui

import java.io.File

import puck.graph._
import puck.graph.io.VisibilitySet

import scala.swing.event.Event

sealed abstract class PuckEvent extends Event

case class GraphFocus(graph : DependencyGraph, edge : DGEdge) extends PuckEvent
case class PushGraph(graph : DependencyGraph) extends PuckEvent
case class PrintErrOrPushGraph(msg : String, lgt : LoggedTry[DependencyGraph]) extends PuckEvent
case class RewriteHistory(rec : Recording) extends PuckEvent
case class NodeClicked(node : DGNode) extends PuckEvent


sealed abstract class GraphStackEvent extends PuckEvent {
  val graph : DependencyGraph
}
case class GraphUpdate(graph : DependencyGraph) extends GraphStackEvent
case class EmptiedButOne(graph : DependencyGraph) extends GraphStackEvent
case class Pushed(pushedGraph : DependencyGraph, previousHead : DependencyGraph) extends GraphStackEvent {
  val graph : DependencyGraph = pushedGraph
}
case class Popped(poppedGraph : DependencyGraph, newHead : DependencyGraph) extends GraphStackEvent {
  val graph : DependencyGraph = newHead
}

case class Log(msg : String ) extends PuckEvent
case class PrintCode(id : NodeId) extends PuckEvent

case object LoadCodeRequest extends PuckEvent
case object LoadConstraintRequest extends PuckEvent

case class SaveRecord(f : File) extends PuckEvent
case class LoadRecord(f : File) extends PuckEvent

case object SwitchView extends PuckEvent
case class ConstraintDisplayRequest(graph : DependencyGraph) extends PuckEvent

case class ApplyOnCodeRequest(searchResult : DependencyGraph) extends PuckEvent
case class GenCode(compareOutput : Boolean) extends PuckEvent



case object PrintingOptionsUpdate extends PuckEvent
sealed abstract class PrintingOptionEvent extends PuckEvent {
  def apply(control : PrintingOptionsControl) : Unit
}

case class VisibilityEvent(graph : DependencyGraph, v : VisibilitySet.T) extends PrintingOptionEvent {
  def apply(control : PrintingOptionsControl) : Unit =
    control.visibility = v
}

case class EdgeForTypePrinting(su : Option[Uses]) extends PrintingOptionEvent {
  def apply(control : PrintingOptionsControl) : Unit =
    control.selectedEdgeForTypePrinting = su
}
