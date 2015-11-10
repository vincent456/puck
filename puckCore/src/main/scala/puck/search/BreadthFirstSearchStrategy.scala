package puck
package search

import puck.graph.LoggedTry
import scala.collection.mutable

class BreadthFirstSearchStrategy[T] extends SearchStrategy[T] {


  val remainingStates = mutable.Queue[SearchState[T]]()

  def currentState = remainingStates.head

  def addState(s : SearchState[T]) : Unit =  remainingStates enqueue s

  def addState(currentResult : LoggedTry[T], choices : Seq[LoggedTry[T]]) : Unit =
    remainingStates enqueue currentState.createNextState(currentResult, choices)

  def canContinue : Boolean =
    !remainingStates.head.triedAll || remainingStates.tail.nonEmpty

  def nextState : SearchState[T] = {
    if (remainingStates.head.triedAll) remainingStates.dequeue()
    remainingStates.head
  }

  def oneStep : Option[( LoggedTry[T], Seq[LoggedTry[T]])] =
    nextState.nextChoice map ((_, Seq()))


}
