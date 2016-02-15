package puck

import puck.graph.transformations.MileStone
import puck.graph.{Recording, DependencyGraph}
import Recording.RecordingOps
import puck.gui._
import puck.util.PuckLogger

import scala.collection.mutable
import scala.swing.Publisher

/**
  * Created by lorilan on 15/12/15.
  */

class GraphStack(val bus : Publisher) {

  def graph = undoStack.head

  def graphOption = undoStack.headOption

  protected val undoStack = mutable.Stack[DependencyGraph]()
  protected val redoStack = mutable.Stack[DependencyGraph]()

  def setInitialGraph(g : DependencyGraph) : Unit = {
    undoStack.clear()
    redoStack.clear()
    undoStack push g
    bus publish GraphUpdate(graph)
  }

  def fireEmptied() : Unit =
    bus publish EmptiedButOne(graph)

  def firePushEvent(previousHead : DependencyGraph) : Unit =
    bus publish Pushed(graph, previousHead)

  def firePopEvent(poppedGraph : DependencyGraph) : Unit =
    bus publish Popped(poppedGraph, graph)

  def canUndo = undoStack.size > 1

  def undoAll() = {

    while(undoStack.nonEmpty) {
      redoStack.push(undoStack.pop())
    }
    undoStack.push(redoStack.pop())
    fireEmptied()
  }

  def undo() = {
    val comments = graph.recording.commentsSinceLastMileStone
    redoStack.push(undoStack.pop())

    val sb = new  mutable.StringBuilder()
    ("Undo " +: comments) foreach (c => sb.append(s"$c\n"))
    bus publish Log(sb.toString)

    firePopEvent(redoStack.head)
  }
  def canRedo = redoStack.nonEmpty

  def redo()={
    val oldHead = undoStack.head
    undoStack.push(redoStack.pop())

    val comments = graph.recording.commentsSinceLastMileStone
    val sb = new  mutable.StringBuilder()
    ("Redo " +: comments) foreach (c => sb.append(s"$c\n"))
    bus publish Log(sb.toString)
    firePushEvent(oldHead)
  }

  def pushGraph(graph: DependencyGraph) = {
    val oldHead = undoStack.head
    undoStack.push(graph)
    redoStack.clear()
    firePushEvent(oldHead)
  }

  def rewriteHistory(rec : Recording): Unit ={
      undoStack.pop()
      pushGraph(rec.redo(graph))
  }

  def load(rec : Recording): Unit =
    pushGraph(rec.reverse.foldLeft(graph){
    case (g, MileStone) =>
      undoStack.push(g)
      MileStone.redo(g)
    case (g, t) => t.redo(g)
  })

}
