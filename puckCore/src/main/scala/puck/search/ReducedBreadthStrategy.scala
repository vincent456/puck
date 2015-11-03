package puck.search


import puck.util.Logged
import scala.collection.mutable



class ReducedBreadthStrategy[T]
( val evaluator : Evaluator[T],
  val reductionPeriod : Int = 2,
  val numNewRoot : Int = 10
  ) extends SearchStrategy[T]{

  val remainingStates = mutable.Stack[SearchState[T]]()

  def canContinue : Boolean = remainingStates.nonEmpty || frozenStates.nonEmpty

  val frozenStates = mutable.Stack[SearchState[T]]()

  def currentState : SearchState[T] = remainingStates.head
  def addState(s : SearchState[T]) : Unit = remainingStates.push(s)

  var currentCheckPoint : Int = reductionPeriod



  override def createState[S <: StateCreator[T, S]](cr : Logged[T], choices : S): Unit ={
    val newState = remainingStates.head.createNextState[S](cr, choices)

    if ( newState.markedPointDepth >= currentCheckPoint)
      frozenStates push newState
    else remainingStates push newState

  }

  var i = 0
  override def oneStep(se : SearchEngine[T]) : Unit = {

    if (remainingStates.nonEmpty) {

      if (remainingStates.head.triedAll) remainingStates.pop()
      else remainingStates.head.executeNextChoice(se)

    }
    else if(frozenStates.nonEmpty) {

      i += 1
      remainingStates pushAll evaluator.bests(frozenStates.toSeq, numNewRoot)
      frozenStates.clear()
      currentCheckPoint += reductionPeriod
      this.oneStep(se)

    }
  }

}

