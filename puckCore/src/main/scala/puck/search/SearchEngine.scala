package puck.search

import puck.graph.LoggedTry
import puck.util.Logged

import scala.collection.mutable
import scalaz._, Scalaz._

trait Search[Result]{
  def initialState : SearchState[Result]
  def successes : Seq[FinalState[Result]]
  def failures : Seq[ErrorState[Result]]
  def exploredStates : Int

  def failuresByDepth : Map[Int, Seq[ErrorState[Result]]] =
    failures.foldLeft(Map[Int, Seq[ErrorState[Result]]]()){
      (m, state) =>
        val seq = m.getOrElse(state.depth, Seq())
        m + (state.depth -> (state +: seq))
    }

  def allStatesByDepth : Map[Int, Seq[SearchState[Result]]] =
    initialState.iterator.foldLeft(Map[Int, Seq[SearchState[Result]]]()){
      (m, state) =>
        val seq = m.getOrElse(state.depth, Seq())
        m + (state.depth -> (state +: seq))
    }
}

object SearchEngine {
  type InitialStateFactory[T] = (LoggedTry[T] => Unit) => SearchState[T]
}

class SearchStateSet[T]{
  val stateStack = mutable.Stack[SearchState[T]]()

  def head : SearchState[T] = stateStack.head

  def removeHead() : Unit ={
    val _ = stateStack.pop()
  }
  def removeAll() : Unit = stateStack.clear()

  def add( ss : SearchState[T]) : Unit ={
    val _ = stateStack.push(ss)
  }
  def addAll(sss : TraversableOnce[SearchState[T]]) : Unit = {
    val _ = stateStack pushAll sss
  }

  def isEmpty : Boolean = stateStack.isEmpty
  def nonEmpty : Boolean = !isEmpty
}


import SearchEngine._
class SearchEngine[T]
( val createInitialState : InitialStateFactory[T],
  val searchStrategy: SearchStrategy[T]
  ) extends Search[T] {

  def currentState : SearchState[T] = searchStrategy.remainingStates.head
  val successes = mutable.ListBuffer[FinalState[T]]()
  val failures = mutable.ListBuffer[ErrorState[T]]()


  private [this] var idSeed : Int = 0
  private def idGen() : Int = {idSeed += 1; idSeed}

  def storeResult(prevState : SearchState[T], res : LoggedTry[T]): Unit = {
    val log = res.log
    res.value match {
      case -\/(err) =>
        failures += new ErrorState[T](idGen(), err.set(log), prevState)
      case \/-(g) =>
        val fs = new FinalState[T](idGen(), g.set(log), Some(prevState))
        successes += fs
        prevState.nextStates += fs
    }

    numExploredStates = numExploredStates + 1
  }

  var initialState : SearchState[T] = _
  protected var numExploredStates = 0

  def addState[S <: StateCreator[T, S]](cr : Logged[T], choices : S)  = {
    numExploredStates = numExploredStates + 1
    searchStrategy.addState(cr, choices)
  }

  def init(k : LoggedTry[T] => Unit) : Unit = {
    initialState = createInitialState(k)
    searchStrategy.remainingStates.add(initialState)
    numExploredStates = 1
  }

  def exploredStates = numExploredStates

  //def startExplore(k : LoggedTry[T] => Unit) : Unit

  def explore() : Unit ={
    init(storeResult(currentState, _))
    do searchStrategy.oneStep(this)
    while(searchStrategy.continue(this))
  }
}

abstract class SearchStrategy[T] {
  val remainingStates = new SearchStateSet[T]()

  def addState(s : SearchState[T]) : Unit =
    remainingStates add s


  def addState[S <: StateCreator[T, S]](cr : Logged[T], choices : S): Unit =
    remainingStates add remainingStates.head.createNextState[S](cr, choices)


  def continue(se : SearchEngine[T]) : Boolean

  def oneStep(se : SearchEngine[T]) : Unit = {
    if (remainingStates.head.triedAll) remainingStates.removeHead()
    else remainingStates.head.executeNextChoice(se)
  }

  def onNewCurrentState(se : SearchEngine[T]) : Unit = ()
}

class TryAllSearchStrategy[T] extends SearchStrategy[T] {
  def continue(se : SearchEngine[T]) : Boolean =
    remainingStates.nonEmpty
}

class FindFirstSearchStrategy[T] extends SearchStrategy[T] {
  def continue(se : SearchEngine[T]) : Boolean =
    remainingStates.nonEmpty && se.successes.isEmpty
}

