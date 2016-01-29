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

case object Undo extends PuckEvent
case object UndoAll extends PuckEvent
case object Redo extends PuckEvent
case object RedoAll extends PuckEvent
case class UndoRedoStatus(canUndo : Boolean, canRedo :Boolean) extends PuckEvent

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
