package puck
package search

import puck.graph.LoggedTry

import scala.collection.mutable

/**
  * Created by lorilan on 16/11/15.
  */
class AStarSearchStrategy[T]
( evaluator: Evaluator [T],
  depthCost : Int = 1
  ) extends SearchStrategy[T] {


  implicit object SearchStateOrdering extends Ordering[SearchState[T]]{

    def evaluateWithDepthPenaly(x: SearchState[T]) =
      Math.max(evaluator.evaluateInt(x) - x.depth * depthCost, 0)

    override def compare(x: SearchState[T], y: SearchState[T]): Int =
      evaluateWithDepthPenaly(x) - evaluateWithDepthPenaly(y)
  }

  val remainingStates = new mutable.PriorityQueue[SearchState[T]]()

  def addState(s: SearchState[T]): Unit =
    ignore(remainingStates += s)

  def currentState: SearchState[T] =
    remainingStates.head

  def addState(currentResult: LoggedTry[T], choices: Seq[LoggedTry[T]]): Unit =
    ignore(remainingStates += currentState.createNextState(currentResult, choices))

  def nextState: SearchState[T] = {
    if (remainingStates.head.triedAll) remainingStates.dequeue()
    remainingStates.head
  }

  def canContinue: Boolean =
    !remainingStates.head.triedAll || remainingStates.tail.nonEmpty
}
