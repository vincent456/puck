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

package puck.control

import java.io.File

import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.io.VisibilitySet
import puck.graph.transformations.Mutability
import puck.view.ViewHandler

import scala.swing.event.Event

sealed abstract class PuckEvent extends Event

case class GraphFocus(graph : DependencyGraph, edge : DGEdge) extends PuckEvent
case class PushGraph(graph : DependencyGraph) extends PuckEvent
case class PrintErrOrPushGraph(msg : String, lgt : LoggedTry[DependencyGraph]) extends PuckEvent
case class RewriteHistory(rec : Recording) extends PuckEvent
case class NodeClicked(node : DGNode) extends PuckEvent

case class ConstraintsUpdate(graph : DependencyGraph, cm : ConstraintsMaps) extends PuckEvent

sealed abstract class GraphChangeEvent extends PuckEvent {
  def graph : DependencyGraph
}
case class GraphUpdate(graph : DependencyGraph) extends GraphChangeEvent
case class Pushed(pushedGraph : DependencyGraph, previousHead : DependencyGraph) extends GraphChangeEvent {
  def graph : DependencyGraph = pushedGraph
}
case class Popped(poppedGraph : DependencyGraph, newHead : DependencyGraph) extends GraphChangeEvent {
  def graph : DependencyGraph = newHead
}

case class Log(msg : String ) extends PuckEvent
case class PrintCode(id : NodeId) extends PuckEvent

case object LoadCodeRequest extends PuckEvent
case object LoadConstraintRequest extends PuckEvent

case class SaveRecord(f : File) extends PuckEvent
case class LoadRecord(f : File) extends PuckEvent

case class ExportGraph(f: File) extends PuckEvent

case class SwitchView(handler : ViewHandler) extends PuckEvent
case class ConstraintDisplayRequest(graph : DependencyGraph) extends PuckEvent
case object EditConstraints extends PuckEvent

case class GenCode(compareOutput : Boolean) extends PuckEvent

case object PrintingOptionsUpdate extends PuckEvent

case class SetMutability(nodeId: NodeId, mutability: Mutability) extends PuckEvent

sealed abstract class PrintingOptionEvent extends PuckEvent {
  def apply(control : PrintingOptionsControl) : Unit
}

//events emitted by the printing option controller
case class VisibilityEvent(graph : DependencyGraph, v : VisibilitySet.T)
  extends PrintingOptionEvent {
  def apply(control : PrintingOptionsControl) : Unit =
    control.visibility = v
}

case class EdgeForTypePrinting(su : Option[NodeIdP])
  extends PrintingOptionEvent {
  def apply(control : PrintingOptionsControl) : Unit =
    control.selectedEdgeForTypePrinting = su
}

sealed abstract class ReceivedByPrintControlEvt extends PrintingOptionEvent
//event received by the printing option controller
case class Hide(graph: DependencyGraph, node : NodeId) extends ReceivedByPrintControlEvt {
  def apply(control : PrintingOptionsControl) : Unit =
    control.hide(graph, node)
}
case class FocusExpand(graph: DependencyGraph, node : NodeId,
                       focus : Boolean, expand : Boolean) extends ReceivedByPrintControlEvt{
  def apply(control : PrintingOptionsControl) : Unit =
    control.focusExpand(graph, node, focus, expand)
}
case class Collapse(graph: DependencyGraph, node : NodeId) extends ReceivedByPrintControlEvt{
  def apply(control : PrintingOptionsControl) : Unit =
    control.collapse(graph, node)
}
case class ExpandAll(graph: DependencyGraph, node : NodeId) extends ReceivedByPrintControlEvt{
  def apply(control : PrintingOptionsControl) : Unit =
    control.expandAll(graph, node)
}