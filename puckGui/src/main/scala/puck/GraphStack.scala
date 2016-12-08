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

import puck.graph.transformations.MileStone
import puck.graph.{DependencyGraph, Recording}
import Recording.RecordingOps
import puck.gui._

import scala.collection.mutable
import scala.swing.{Component, Publisher}

/**
  * Created by Loïc Girault on 15/12/15.
  */

class GraphStack(val bus : Publisher) extends HistoryHandler {

  override def graph = undoStack.head

  def graphOption = undoStack.headOption

  protected val undoStack = mutable.Stack[DependencyGraph]()
  protected val redoStack = mutable.Stack[DependencyGraph]()

  override def setInitialGraph(g : DependencyGraph) : Unit = {
    undoStack.clear()
    redoStack.clear()
    ignore(undoStack push g)
    bus publish GraphUpdate(g)
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

  override def pushGraph(graph: DependencyGraph) = {
    val oldHead = undoStack.head
    undoStack.push(graph)
    redoStack.clear()
    firePushEvent(oldHead)
  }

  override def rewriteHistory(rec : Recording): Unit ={
    undoStack.pop()
    pushGraph(rec.redo(graph))
  }

  override def load(rec : Recording): Unit = {
    val g = rec.reverse.foldLeft(graph) {
      case (g, MileStone) =>
        undoStack.push(g)
        MileStone.redo(g)
      case (g, t) => t.redo(g)
    }
    if(!(g eq graph))
      undoStack push g

    bus publish GraphUpdate(graph)
  }

  def view() : Component = new UndoRedoPane(this, bus)
}
