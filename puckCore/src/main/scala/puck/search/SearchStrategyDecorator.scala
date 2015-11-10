package puck.search

import puck.graph._

/**
 * Created by lorilan on 10/11/15.
 */
class SearchStrategyDecorator[T]
(val strategy : SearchStrategy[T])
  extends SearchStrategy[T] {
     override def addState(s: SearchState[T]): Unit =
       strategy.addState(s)

     override def addState(currentResult: LoggedTry[T],
                           choices: Seq[LoggedTry[T]]): Unit =
       strategy.addState(currentResult, choices)

     override def nextState: SearchState[T] = strategy.nextState

     override def canContinue: Mutability = strategy.canContinue

     override def currentState: SearchState[T] = strategy.currentState

     override def oneStep: Option[(LoggedTry[T], Seq[LoggedTry[T]])] = strategy.oneStep
   }
