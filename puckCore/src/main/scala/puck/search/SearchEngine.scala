/*
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
import SearchEngine._
class SearchEngine[T]
( val createInitialState : InitialStateFactory[T],
  val searchStrategy: SearchStrategy[T],
  val maxResult : Option[Int] = None // default = all result
  ) extends Search[T] {

  val successes = mutable.ListBuffer[FinalState[T]]()
  val failures = mutable.ListBuffer[ErrorState[T]]()

  val enoughSuccess : () => Boolean =
    maxResult match {
      case None => () => false
      case Some(i) => () => successes.length >= i
    }

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
    searchStrategy.createState(cr, choices)
  }

  def init(k : LoggedTry[T] => Unit) : Unit = {
    initialState = createInitialState(k)
    searchStrategy.addState(initialState)
    numExploredStates = 1
  }

  def exploredStates = numExploredStates

  def explore() : Unit = {
    init(storeResult(searchStrategy.currentState, _))
    do searchStrategy.oneStep(this)
    while(searchStrategy.canContinue && !enoughSuccess())
  }


}

trait SearchStrategy[T] {
  def currentState : SearchState[T]
  def addState(s : SearchState[T]) : Unit
  def createState[S <: StateCreator[T, S]](currentResult : Logged[T], choices : S) : Unit
  def canContinue : Boolean
  def oneStep(se : SearchEngine[T]) : Unit
}




*/
