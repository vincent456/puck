package puck.searchNew

import puck.graph.LoggedTry

import scala.collection.mutable
import scalaz.{\/-, -\/}

trait Search[Result]{
  def initialState : SearchState[Result]
  def successes : Seq[SearchState[Result]]
  def failures : Seq[SearchState[Result]]
  def exploredStates : Int

  def failuresByDepth : Map[Int, Seq[SearchState[Result]]] =
    failures.foldLeft(Map[Int, Seq[SearchState[Result]]]()){
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


class SearchEngine[T]
( val initialState : SearchState[T],
  val searchStrategy: SearchStrategy[T],
  val maxResult : Option[Int] = None // default = all result
  ) extends Search[T] {

  val successes = mutable.ListBuffer[SearchState[T]]()
  val failures = mutable.ListBuffer[SearchState[T]]()

  val enoughSuccess : () => Boolean =
    maxResult match {
      case None => () => false
      case Some(i) => () => successes.length >= i
    }

  private [this] var idSeed : Int = 0
  private def idGen() : Int = {idSeed += 1; idSeed}

  def storeResult(prevState : SearchState[T], res : LoggedTry[T]): Unit = {
    res.value match {
      case -\/(err) =>
        failures += prevState.createNextState(res, Seq())
      case \/-(g) =>
        successes += prevState.createNextState(res, Seq())
    }
    numExploredStates = numExploredStates + 1
  }

  protected var numExploredStates = 0

  def addState(cr : LoggedTry[T], choices : Seq[LoggedTry[T]]) :Unit = {
    numExploredStates = numExploredStates + 1
    searchStrategy.addState(cr, choices)
  }

  def init() : Unit = {
  }

  def exploredStates = numExploredStates

  def explore() : Unit = {

    searchStrategy.addState(initialState)
    numExploredStates = 1

    do searchStrategy.oneStep foreach {
      case (res, choices) => addState(res, choices)
    }
    while(searchStrategy.canContinue && !enoughSuccess())
  }


}

trait SearchStrategy[T] {
  def addState(s : SearchState[T]) : Unit
  def addState(currentResult : LoggedTry[T], choices : Seq[LoggedTry[T]]) : Unit
  def canContinue : Boolean
  def currentState : SearchState[T]
  def nextState: SearchState[T]
  def oneStep : Option[(LoggedTry[T], Seq[LoggedTry[T]])]
}




