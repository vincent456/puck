package puck.search

import puck.graph.LoggedTry
import scala.collection.mutable

class DepthFirstSearchStrategy[T] extends SearchStrategy[T] {


  val remainingStates = mutable.Stack[SearchState[T]]()

  def currentState = remainingStates.head

  private def push(s : SearchState[T]) : Unit = {
    val _ = remainingStates push s
  }

  def addState(s : SearchState[T]) : Unit = push(s)

  def addState(currentResult : LoggedTry[T], choices : Seq[LoggedTry[T]]) : Unit =
    this push currentState.createNextState(currentResult, choices)

  def canContinue : Boolean =
  !remainingStates.head.triedAll || remainingStates.tail.nonEmpty

  def nextState : SearchState[T] = {
    if (remainingStates.head.triedAll) remainingStates.pop()
    remainingStates.head
  }

}