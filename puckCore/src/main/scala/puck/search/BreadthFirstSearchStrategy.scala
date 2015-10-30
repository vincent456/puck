package puck.search

import puck.util._

import scala.collection.mutable

class BreadthFirstSearchStrategy[T] extends SearchStrategy[T] {


  val remainingStates = mutable.Stack[SearchState[T]]()

  val depthPlusOneStates = mutable.Stack[SearchState[T]]()

  def currentState = remainingStates.head

  private def push(s : SearchState[T]) : Unit = {
    val _ = depthPlusOneStates push s
  }

  def addState(s : SearchState[T]) : Unit = push(s)

  def createState[S <: StateCreator[T, S]](currentResult : Logged[T], choices : S) : Unit =
    this push remainingStates.head.createNextState[S](currentResult, choices)

  def continue(se : SearchEngine[T]) : Boolean =
    (remainingStates.nonEmpty || depthPlusOneStates.nonEmpty) && se.successes.isEmpty

  def oneStep(se : SearchEngine[T]) : Unit = {

    if(remainingStates.isEmpty) {
      remainingStates pushAll depthPlusOneStates
      depthPlusOneStates.clear()
    }

    if (remainingStates.head.triedAll) remainingStates.pop()
    else remainingStates.head.executeNextChoice(se)

  }

}

//val stateStack =
//
//def head : SearchState[T] = stateStack.head
//
//def removeHead() : Unit ={
//val _ = stateStack.pop()
//}
//def removeAll() : Unit = stateStack.clear()
//
//def add( ss : SearchState[T]) : Unit ={
//val _ = stateStack.push(ss)
//}
//def addAll(sss : TraversableOnce[SearchState[T]]) : Unit = {
//val _ = stateStack pushAll sss
//}
//
//def isEmpty : Boolean = stateStack.isEmpty
//def nonEmpty : Boolean = !isEmpty