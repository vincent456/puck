package puck.search

import scala.collection.mutable

/**
 * Created by lorilan on 07/07/14.
 */

trait SearchEngine[S, U]{

  val initialState : SearchState[S, U]

  var currentState : SearchState[S, U] = _

  val finalStates = mutable.ListBuffer[SearchState[S, U]]()

  def start(){
    currentState = initialState
    currentState.executeNextChoice()
  }

  def keepGoing() { currentState.executeNextChoice() }


  def newCurrentState(k: U => Unit,
                      choices : S) {

    //only one choice : no state created
    /*if(it.isEmpty)
      k(None)
    else {
      if (remainingChoices.size == 1)
        k(Some(remainingChoices.head))
      else {*/
    val newState = currentState.createNextState(k, choices)
    currentState = newState
    keepGoing()
    /*  }
    }*/
  }


  def search() : Option[SearchState[S, U]]
}


trait TryAllSearchEngine[S, U] extends SearchEngine[S, U]{

  val stateStack = mutable.Stack[SearchState[S, U]]()

  override def search() = {
    start()
    while(stateStack.nonEmpty){
      if(stateStack.head.triedAll)
        stateStack.pop()
      else {
        val state = stateStack.head
        println("#########################################################################################")
        println("#########################################################################################")
        println("#########################################################################################")
        println("EXPLORING FROM " + state.uuid("/","_",""))

        /* state.prevState match {
           case None => ()
           case Some(s) => println("PREVSTATE    : " + s.uuid("/","_","") )
         }*/

        stateStack.head.executeNextChoice()
      }
    }
    None
  }

  override def keepGoing(){
    stateStack.push(currentState)
    currentState.executeNextChoice()
  }

}

trait GradedSearchEngine[S, U] extends SearchEngine[S, U]{

  def grade(state : SearchState[S, U]) : Int

  override def search() : Option[SearchState[S, U]] = {
    val states = mutable.Buffer[SearchState[S, U]]()

    def selectBest() = {
      val (choosedState, _) = states.tail.foldLeft[(SearchState[S, U], Int)]((states.head, grade(states.head))){
        case ((bestState, bestGrade), st) =>
          val gr = grade(st)
          if(gr > bestGrade) (st, gr)
          else (bestState, bestGrade)
      }
      choosedState
    }


    start()
    var prev = currentState


    while(finalStates.isEmpty && !currentState.triedAll) {
      states.clear()
      while (!prev.triedAll) {
        prev.setAsCurrentState()
        prev.executeNextChoice()
        states.append(currentState)
      }

      println("choosing between %d solutions".format(states.length))
      states.foreach(st => println(grade(st)))

      prev = selectBest()
    }

    states.clear()

    states ++= finalStates

    println("final states ! choosing between %d solutions".format(states.length))
    states.foreach(st => println(grade(st)))

    if(finalStates.nonEmpty)
      Some(selectBest())
    else
      None
  }



}

trait FindFirstSearchEngine[S, U] extends SearchEngine[S, U] {

  val stateStack = mutable.Stack[SearchState[S, U]]()

  override def search() : Option[SearchState[S, U]] = {
    start()
    while(stateStack.nonEmpty && finalStates.isEmpty){
      if(stateStack.head.triedAll)
        stateStack.pop()
      else {
        /*   val state = stateStack.head
           println("#########################################################################################")
           println("#########################################################################################")
           println("#########################################################################################")
           println("EXPLORING FROM " + state.uuid("/","_",""))
           println(state.triedChoices.mkString("tried :\n", "\n", "\n"))
           println(state.remainingChoices.mkString("remaining choices :\n", "\n", "\n"))

           state.prevState match {
             case None => ()
             case Some(s) => println("PREVSTATE    : " + s.uuid("/","_","") )
           }
   */
        stateStack.head.executeNextChoice()
      }
    }
    if(finalStates.nonEmpty)
      Some(finalStates.head)
    else
      None
  }

  override def keepGoing(){
    stateStack.push(currentState)
    currentState.executeNextChoice()
  }
}
