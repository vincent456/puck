package puck
package gui

import java.awt.Container
import java.io.File

import puck.graph._
import puck.graph.io.{PrintingOptions, VisibilitySet}
import puck.gui.svg._

import scala.swing.Publisher
import scala.swing.event.Event

sealed abstract class PuckEvent extends Event

case class GraphUpdate(graph : DependencyGraph) extends PuckEvent
case class GraphFocus(graph : DependencyGraph, edge : DGEdge) extends PuckEvent
case class PushGraph(graph : DependencyGraph) extends PuckEvent
case class PrintErrOrPushGraph(msg : String, lgt : LoggedTry[DependencyGraph]) extends PuckEvent
case class RewriteHistory(rec : Recording) extends PuckEvent
case class NodeClicked(node : DGNode) extends PuckEvent

case object Undo extends PuckEvent
case object UndoAll extends PuckEvent
case object Redo extends PuckEvent
case object RedoAll extends PuckEvent
case class UndoRedoStatus(canUndo : Boolean, canRedo :Boolean) extends PuckEvent

case class Log(msg : String ) extends PuckEvent

case object LoadCodeRequest extends PuckEvent
case object LoadConstraintRequest extends PuckEvent

case class SaveRecord(f : File) extends PuckEvent
case class LoadRecord(f : File) extends PuckEvent

case class GraphDisplayRequest
(title : String,
 graph : DependencyGraph,
 visibility : VisibilitySet.T,
 sUse : Option[Uses] = None)
  extends PuckEvent
case class ConstraintDisplayRequest(graph : DependencyGraph) extends PuckEvent

case class ApplyOnCodeRequest(searchResult : DependencyGraph) extends PuckEvent
case class GenCode(compareOutput : Boolean) extends PuckEvent


case object PrintingOptionsUpdate extends PuckEvent
sealed abstract class PrintingOptionEvent extends PuckEvent {
  def apply(control : PrintingOptionsControl) : Unit
}

case class SignatureVisible(b : Boolean) extends PrintingOptionEvent {
  def apply(control : PrintingOptionsControl) : Unit =
    control.signatureVisible = b
}
case class IdVisible(b : Boolean) extends PrintingOptionEvent {
  def apply(control : PrintingOptionsControl) : Unit =
    control.idVisible = b
}
case class VirtualEdgeVisible(b : Boolean) extends PrintingOptionEvent{
  def apply(control : PrintingOptionsControl) : Unit =
    control.virtualEdgesVisible = b
}
case class ConcreteUsePerVirtualEdgeVisible(b : Boolean) extends PrintingOptionEvent{
  def apply(control : PrintingOptionsControl) : Unit =
    control.concreteUsesPerVirtualEdges = b
}
case class RedOnly(b : Boolean) extends PrintingOptionEvent{
  def apply(control : PrintingOptionsControl) : Unit =
    control.redEdgesOnly = b
}
case class EdgeForTypePrinting(su : Option[Uses]) extends PrintingOptionEvent {
  def apply(control : PrintingOptionsControl) : Unit =
    control.selectedEdgeForTypePrinting = su
}



object PuckEvents{
  def addUndoRedoButton(c : Container, publisher: Publisher) :
  UndoRedoStatus => Unit = {
    val undoAllButton = jbutton("Undo all") {
      _ => publisher publish UndoAll
    }
    undoAllButton.setEnabled(false)
    c add undoAllButton

    val undoButton = jbutton("Undo") {
      _ => publisher publish Undo
    }
    undoButton.setEnabled(false)
    c add undoButton
    val redoButton = jbutton("Redo") {
      _ => publisher publish Redo
    }
    redoButton.setEnabled(false)
    c add redoButton

    { case UndoRedoStatus(canUndo, canRedo) =>
        undoAllButton.setEnabled(canUndo)
        undoButton.setEnabled(canUndo)
        redoButton.setEnabled(canRedo)
    }
  }

  def addVisibilityCheckBoxes(c : Container,
                              publisher : Publisher,
                              printingOptions: PrintingOptions) : Unit  = {
    def addCheckBox(name: String, initiallySelected: Boolean)(f: Boolean => Unit) =
      c add checkBox (name, initiallySelected) (f)

    addCheckBox ("Show signatures",
      printingOptions.printSignatures) {
      b => publisher.publish (SignatureVisible (b) )
    }

    addCheckBox ("Show ids",
      printingOptions.printId) {
      b => publisher.publish (IdVisible (b) )
    }
    addCheckBox ("Show Virtual Edges",
      printingOptions.printVirtualEdges) {
      b => publisher.publish (VirtualEdgeVisible (b) )
    }
    addCheckBox ("Concrete Uses/Virtual Edge",
      printingOptions.printConcreteUsesPerVirtualEdges) {
      b => publisher.publish (ConcreteUsePerVirtualEdgeVisible (b) )
    }
    ignore(addCheckBox ("Show RedOnly",
      printingOptions.redOnly) {
      b => publisher.publish (RedOnly (b) )
    })
  }

  def addLoadSaveButton(c : Container, publisher : Publisher, currentDir : File) : Unit = {
    c add jbutton("Save refactoring plan") {
      _ =>
        saveFile(currentDir, c) match {
          case None => publisher publish Log("no file selected")
          case Some(f) =>  publisher publish SaveRecord(f)
        }
    }
    c add jbutton("Load refactoring plan") {
      _ =>
        openFile(currentDir, c) match {
          case None => publisher publish Log("no file selected")
          case Some(f) => publisher publish LoadRecord(f)
        }
    } ; ()
  }
}