package puck.search

import scala.collection.mutable

/**
 * Created by lorilan on 07/07/14.
 */

trait SearchEngine[F <: StateCreator[F, F]]{

  val initialState : SearchState[_, F]

  var currentState : SearchState[_, F] = _

  val finalStates = mutable.ListBuffer[SearchState[F, F]]()

  def init(){
    currentState = initialState
  }

  def newCurrentState[S <: StateCreator[S, F]](choices : S) {
    currentState = currentState.createNextState[S](choices)
  }

  def search() : Option[SearchState[F, F]]
}


trait StackedSearchEngine[F <: StateCreator[F, F]] extends SearchEngine[F]{

  val stateStack = mutable.Stack[SearchState[_, F]]()

  override def init(){
    //println("StackedSearchEngine.init")
    super.init()
    stateStack.push(initialState)
  }

  override def newCurrentState[S <: StateCreator[S, F]](choices : S) {
    //println("StackedSearchEngine.newCurrentState")
    super.newCurrentState(choices)
    stateStack.push(currentState)

  }
}

trait TryAllSearchEngine[F <: StateCreator[F, F]] extends StackedSearchEngine[F]{

  override def search() = {
    init()

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

        stateStack.head.executeNextChoice()
      }
    }
    None
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

trait FindFirstSearchEngine[F <: StateCreator[F, F]] extends StackedSearchEngine[F] {

  override def search() = {
    init()
    while(stateStack.nonEmpty && finalStates.isEmpty){
      if(stateStack.head.triedAll)  //curentState
        stateStack.pop()
      else {
         /*val state = stateStack.head
           println("#########################################################################################")
           println("#########################################################################################")
           println("#########################################################################################")
           println("EXPLORING FROM " + state.uuid("/","_",""))

           state.prevState match {
             case None => ()
             case Some(s) => println("PREVSTATE    : " + s.uuid("/","_","") )
           }*/
        stateStack.head.executeNextChoice()
      }
    }
    if(finalStates.nonEmpty)
      Some(finalStates.head)
    else
      None
  }

}
