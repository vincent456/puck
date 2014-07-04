package puck.javaAG

import puck.graph.{AccessGraph, NodeKind}
import puck.graph.constraints.{SearchState, Solver, SearchEngine}
import puck.javaAG.JavaNodeKind.{Interface, Class, Constructor, Field}

import scala.collection.mutable

/**
 * Created by lorilan on 02/07/14.
 */
class JavaSearchEngine(g : AccessGraph,
                       val printTrace : SearchState => Unit) extends SearchEngine {

  val solver : Solver = new JavaSolver(g, this)
  val violationsKindPriority = List[NodeKind](Field(), Constructor(),
    Class(), Interface())



  val stateStack = mutable.Stack[SearchState]()

  def explore(){
    graph.transformations.startRegister()
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
        println(state.triedChoices.mkString("tried :\n", "\n", "\n"))
        println(state.remainingChoices.mkString("remaining choices :\n", "\n", "\n"))

        state.prevState match {
          case None => ()
          case Some(s) => println("PREVSTATE    : " + s.uuid("/","_","") )
        }


        stateStack.head.executeNextChoice()
      }


    }
  }

  override def keepGoing(){
    stateStack.push(currentState)
    /*initialState.iterator.find(s =>
      !(s eq currentState) &&
        s.recording.produceSameGraph(currentState.recording)
    ) match {
      case Some(_) => println("does not execute next choice : other state producing same graph found")
      case None => currentState.executeNextChoice()
    }*/

    currentState.executeNextChoice()
  }
}
