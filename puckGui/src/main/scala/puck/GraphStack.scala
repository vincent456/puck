package puck

import puck.graph.DependencyGraph
import puck.util.PuckLogger

import scala.collection.mutable

/**
  * Created by lorilan on 15/12/15.
  */

trait StackListener{
  def update(svgController: GraphStack) : Unit
}
trait GraphStack {

  val initialGraph : DependencyGraph
  val logger : PuckLogger

  def graph =
    if(undoStack.nonEmpty) undoStack.head
    else initialGraph

  protected val undoStack = mutable.Stack[DependencyGraph]()
  protected val redoStack = mutable.Stack[DependencyGraph]()

  private val stackListeners = mutable.ArrayBuffer[StackListener]()

  def registerAsStackListeners(l : StackListener) =
    stackListeners.append(l)

  def updateStackListeners() : Unit =
    stackListeners.foreach(_.update(this))

  def canUndo = undoStack.nonEmpty

  def undoAll() = {

    while(undoStack.nonEmpty)
      redoStack.push(undoStack.pop())
    updateStackListeners()
  }

  def undo() = {
    val comments = graph.recording.commentsSinceLastMileStone
    redoStack.push(undoStack.pop())

    ("Undo " +: comments) foreach (logger.writeln(_))
    updateStackListeners()
  }
  def canRedo = redoStack.nonEmpty

  def redo()={
    undoStack.push(redoStack.pop())

    val comments = graph.recording.commentsSinceLastMileStone
    ("Redo " +: comments) foreach (logger.writeln(_))
    updateStackListeners()
  }

  def pushGraph(graph: DependencyGraph) = {
    undoStack.push(graph)
    redoStack.clear()

    //console.displayWeight(Metrics.weight(graph, graph.nodeKindKnowledge.lightKind))

    updateStackListeners()
  }


}
