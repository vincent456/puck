package puck.search

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
 * Created by lorilan on 07/07/14.
 */

trait Search[Result]{
  def initialState : SearchState[Result]
  def finalStates : Seq[FinalState[Result]]
  def exploredStates : Int
}

trait SearchEngine[T] extends Search[T]{

  var currentState : SearchState[T] = _
  override val finalStates = mutable.ListBuffer[SearchState[T]]()

  private [this] var idSeed : Int = 0
  private def idGen() : Int = {idSeed += 1; idSeed}

  def storeResult(prevState : Option[SearchState[T]], res : T): Unit = {
    finalStates += new FinalState[T](idGen(), res, this, prevState)
  }

  private var numExploredStates = 0

  def init(){
    currentState = initialState
    numExploredStates = 1
  }

  def newCurrentState[S <: StateCreator[T, S]](cr : T, choices : S)  = {
    currentState = currentState.createNextState[S](cr, choices)
    numExploredStates = numExploredStates + 1
  }

  def search(k : Try[T] => Unit) {
    init()
    currentState.executeNextChoice(k)
  }

  def exploredStates = numExploredStates

  def doExplore(k : Try[T] => Unit) : Unit

  def explore() : Unit ={
    doExplore {
      case Success(result) =>storeResult(Some(currentState), result)
      case Failure(e) => println(e.getMessage)
    }
  }
}


trait StackedSearchEngine[Result] extends SearchEngine[Result]{

  val stateStack = mutable.Stack[SearchState[Result]]()

  override def init(){
    //println("StackedSearchEngine.init")
    super.init()
    stateStack.push(initialState)
  }

  override def newCurrentState[S <: StateCreator[Result, S]](cr : Result, choices : S) {
    //println("StackedSearchEngine.newCurrentState")
    super.newCurrentState(cr, choices)
    stateStack.push(currentState)

  }
}

trait TryAllSearchEngine[ResT] extends StackedSearchEngine[ResT]{

  def doExplore( k : Try[ResT] => Unit) {

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

        stateStack.head.executeNextChoice(k)
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

trait FindFirstSearchEngine[T] extends StackedSearchEngine[T] {

  def doExplore( k : Try[T] => Unit) {
     this.search(k)

     while(stateStack.nonEmpty && finalStates.isEmpty){
        if(stateStack.head.triedAll) stateStack.pop()
        else  stateStack.head.executeNextChoice(k)
     }
  }
}

