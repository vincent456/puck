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
}


trait TryAllSearchEngine[S, U] extends SearchEngine[S, U]{

  val stateStack = mutable.Stack[SearchState[S, U]]()

  def explore(){
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
  }

  override def keepGoing(){
    stateStack.push(currentState)
    currentState.executeNextChoice()
  }

}

trait FindFirstSearchEngine[S, U] extends SearchEngine[S, U] {

  val stateStack = mutable.Stack[SearchState[S, U]]()

  def search() : Option[SearchState[S, U]] = {
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
