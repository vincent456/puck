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
trait SearchEngine[T] extends Search[T] {

  var currentState : SearchState[T] = _
  override val successes = mutable.ListBuffer[FinalState[T]]()
  override val failures = mutable.ListBuffer[ErrorState[T]]()


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

  val createInitialState : InitialStateFactory[T]

  def newCurrentState[S <: StateCreator[T, S]](cr : Logged[T], choices : S)  = {
    currentState = currentState.createNextState[S](cr, choices)
    numExploredStates = numExploredStates + 1
  }

  def init(k : LoggedTry[T] => Unit) : Unit = {
    initialState = createInitialState(k)
    currentState = initialState
    numExploredStates = 1
  }

  def search(k : LoggedTry[T] => Unit) : Unit =  {
    init(k)
    currentState.executeNextChoice(this)
  }

  def exploredStates = numExploredStates

  protected def startExplore(k : LoggedTry[T] => Unit) : Unit

  def explore() : Unit ={
    startExplore {
      storeResult(currentState, _)
    }
  }
}


abstract class StackedSearchEngine[Result]
  extends SearchEngine[Result]{

  val stateStack = mutable.Stack[SearchState[Result]]()

  override def init(k : LoggedTry[Result] => Unit): Unit = {
    //println("StackedSearchEngine.init")
    super.init(k)
    stateStack.push(initialState)
    ()
  }

  override def newCurrentState[S <: StateCreator[Result, S]](cr : Logged[Result], choices : S) : Unit =  {
    //println("StackedSearchEngine.newCurrentState")
    super.newCurrentState(cr, choices)
    stateStack.push(currentState)
    ()
  }
}

class TryAllSearchEngine[T]
( val createInitialState : InitialStateFactory[T]
) extends StackedSearchEngine[T]{

  protected def startExplore( k : LoggedTry[T] => Unit) : Unit =  {

  this.search(k)

  while(stateStack.nonEmpty){
      if(stateStack.head.triedAll)
        stateStack.pop()
      else {
        stateStack.head.executeNextChoice(this)
      }
    }
  }
}

//trait GradedSearchEngine[S] extends SearchEngine[S]{
//
//  def grade(state : SearchState[S, U]) : Int
//
//  override def search() : Option[SearchState[S, U]] = {
//    val states = mutable.Buffer[(SearchState[S, U], Int)]()
//
//    def selectBest() = {
//      val (choosedState, _) = states.tail.foldLeft[(SearchState[S, U], Int)](states.head){
//        case ((bestState, bestGrade), (state, grade)) =>
//          if(grade > bestGrade) (state, grade)
//          else (bestState, bestGrade)
//      }
//      choosedState
//    }
//
//
//    init()
//    var prev = currentState
//
//
//    while(finalStates.isEmpty && !currentState.triedAll) {
//      states.clear()
//      while (!prev.triedAll) {
//        prev.setAsCurrentState()
//        prev.executeNextChoice()
//        states.append((currentState, grade(currentState)))
//      }
//
//      println("choosing between %d solutions".format(states.length))
//      states.foreach{case (_, grade) => println(grade)}
//
//      prev = selectBest()
//    }
//
//    states.clear()
//
//    states ++= finalStates.map{
//      st =>
//        st.setAsCurrentState()
//        (st, grade(st))
//    }
//
//    println("final states ! choosing between %d solutions".format(states.length))
//    states.foreach{
//      case (_, grade) => println(grade)
//    }
//
//    if(finalStates.nonEmpty)
//      Some(selectBest())
//    else
//      None
//  }
//}

class FindFirstSearchEngine[T]
( val createInitialState : InitialStateFactory[T]
  ) extends StackedSearchEngine[T] {

  protected def startExplore( k : LoggedTry[T] => Unit): Unit = {

    this.search(k)
    while(stateStack.nonEmpty && successes.isEmpty){
        if(stateStack.head.triedAll) stateStack.pop()
        else stateStack.head.executeNextChoice(this)
     }

  }
}

