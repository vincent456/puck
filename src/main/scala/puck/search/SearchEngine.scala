package puck.search

import scala.collection.mutable
import scala.util.continuations.reset

/**
 * Created by lorilan on 07/07/14.
 */

trait Search[Result]{
  def initialState : SearchState[Result]
  def finalStates : Seq[FinalState[Result]]
  def exploredStates : Int
}

trait SearchEngine[Result] extends Search[Result]{

  var currentState : SearchState[Result] = _
  override val finalStates = mutable.ListBuffer[SearchState[Result]]()

  private [this] var idSeed = 0
  private def idGen() : Int = {idSeed += 1; idSeed}

  def storeResult(prevState : Option[SearchState[Result]], res : Result): Unit = {
    finalStates += new FinalState[Result](idGen(), res, this, prevState)
  }

  private var numExploredStates = 0

  def init(){
    currentState = initialState
    numExploredStates = 1
  }

  def newCurrentState[S <: StateCreator[Result, S]](cr : Result, choices : S)  = {
    currentState = currentState.createNextState[S](cr, choices)
    numExploredStates = numExploredStates + 1
  }

  def search() : Option[Result] = {
    init()
    currentState.executeNextChoice()
  }

  def exploredStates = numExploredStates

  def doExplore() : Unit

  def explore() : Unit ={
    reset { doExplore() }
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

trait TryAllSearchEngine[Result] extends StackedSearchEngine[Result]{

  def doExplore() {

    this.search() match {
      case Some(res) => storeResult(Some(stateStack.head), res)
      case None => ()
    }

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

        stateStack.head.executeNextChoice() match {
          case Some(res) => storeResult(Some(stateStack.head), res)
          case None => ()
        }
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

trait FindFirstSearchEngine[Result] extends StackedSearchEngine[Result] {

  def doExplore() {

   this.search() match {
      case Some(res) => storeResult(Some(stateStack.head), res)
      case None =>
        while(stateStack.nonEmpty && finalStates.isEmpty){
          if(stateStack.head.triedAll)
            stateStack.pop()
          else {
            stateStack.head.executeNextChoice() match {
              case Some(res) => storeResult(Some(stateStack.head), res)
              case None => ()
            }
          }
        }
    }
  }

}

