package puck.search

import scala.collection.mutable
import scalaz.{-\/, \/-}

trait Search[Result]{
  def initialState : SearchState[Result]
  def finalStates : Seq[FinalState[Result]]
  def exploredStates : Int

  def allStatesByDepth : Map[Int, Seq[SearchState[Result]]] =
    initialState.iterator.foldLeft(Map[Int, Seq[SearchState[Result]]]()){
    (m, state) =>
      val seq = m.getOrElse(state.depth, Seq())
      m + (state.depth -> (state +: seq))
  }
}

object SearchEngine {
  type InitialStateFactory[T] = (Try[T] => Unit) => SearchState[T]
}
import SearchEngine._
trait SearchEngine[T] extends Search[T]{

  var currentState : SearchState[T] = _
  override val finalStates = mutable.ListBuffer[FinalState[T]]()

  private [this] var idSeed : Int = 0
  private def idGen() : Int = {idSeed += 1; idSeed}

  def storeResult(prevState : Option[SearchState[T]], res : T): Unit = {
    finalStates += new FinalState[T](idGen(), res, this, prevState)
    numExploredStates = numExploredStates + 1
  }

  var initialState : SearchState[T] = _
  protected var numExploredStates = 0

  val createInitialState : InitialStateFactory[T]

  def newCurrentState[S <: StateCreator[T, S]](cr : T, choices : S)  = {
    currentState = currentState.createNextState[S](cr, choices)
    numExploredStates = numExploredStates + 1
  }

  def init(k : Try[T] => Unit) : Unit = {
    initialState = createInitialState(k)
    currentState = initialState
    numExploredStates = 1
  }

  def search(k : Try[T] => Unit) : Unit =  {
    init(k)
    currentState.executeNextChoice(this)
  }

  def exploredStates = numExploredStates

  protected def startExplore(k : Try[T] => Unit) : Unit

  def explore() : Unit ={
    startExplore {
      case \/-(result) => storeResult(Some(currentState), result)
      case -\/(e) => println(e.getMessage)
    }
  }
}


abstract class StackedSearchEngine[Result]
  extends SearchEngine[Result]{

  val stateStack = mutable.Stack[SearchState[Result]]()

  override def init(k : Try[Result] => Unit): Unit = {
    //println("StackedSearchEngine.init")
    super.init(k)
    stateStack.push(initialState)
    ()
  }

  override def newCurrentState[S <: StateCreator[Result, S]](cr : Result, choices : S) : Unit =  {
    //println("StackedSearchEngine.newCurrentState")
    super.newCurrentState(cr, choices)
    stateStack.push(currentState)
    ()
  }
}

class TryAllSearchEngine[ResT]
( val createInitialState : (Try[ResT] => Unit) => SearchState[ResT]
) extends StackedSearchEngine[ResT]{

  protected def startExplore( k : Try[ResT] => Unit) : Unit =  {

  this.search(k)

  while(stateStack.nonEmpty){
      if(stateStack.head.triedAll)
        stateStack.pop()
      else {
/*
        println("#########################################################################################")
        println("#########################################################################################")
        println("#########################################################################################")
        println("EXPLORING FROM " + stateStack.head.uuid("/","_",""))*/

        /* stateStack.head.prevState match {
           case None => ()
           case Some(s) => println("PREVSTATE    : " + s.uuid("/","_","") )
         }*/

        stateStack.head.executeNextChoice(this)
      }
    }
  }
}

/*trait GradedSearchEngine[S] extends SearchEngine[S]{

  def grade(state : SearchState[S, U]) : Int

  override def search() : Option[SearchState[S, U]] = {
    val states = mutable.Buffer[(SearchState[S, U], Int)]()

    def selectBest() = {
      val (choosedState, _) = states.tail.foldLeft[(SearchState[S, U], Int)](states.head){
        case ((bestState, bestGrade), (state, grade)) =>
          if(grade > bestGrade) (state, grade)
          else (bestState, bestGrade)
      }
      choosedState
    }


    init()
    var prev = currentState


    while(finalStates.isEmpty && !currentState.triedAll) {
      states.clear()
      while (!prev.triedAll) {
        prev.setAsCurrentState()
        prev.executeNextChoice()
        states.append((currentState, grade(currentState)))
      }

      println("choosing between %d solutions".format(states.length))
      states.foreach{case (_, grade) => println(grade)}

      prev = selectBest()
    }

    states.clear()

    states ++= finalStates.map{
      st =>
        st.setAsCurrentState()
        (st, grade(st))
    }

    println("final states ! choosing between %d solutions".format(states.length))
    states.foreach{
      case (_, grade) => println(grade)
    }

    if(finalStates.nonEmpty)
      Some(selectBest())
    else
      None
  }
}*/

class FindFirstSearchEngine[T]
( val createInitialState : (Try[T] => Unit) => SearchState[T]
  ) extends StackedSearchEngine[T] {

  protected def startExplore( k : Try[T] => Unit): Unit = {

    this.search(k)
    while(stateStack.nonEmpty && finalStates.isEmpty){
        if(stateStack.head.triedAll) stateStack.pop()
        else stateStack.head.executeNextChoice(this)
     }

  }
}

