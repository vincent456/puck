package puck.graph
package constraints.search

import puck.search.{SearchEngine, StateCreator, SearchState}
import puck.util.Logged

import scalaz.syntax.writer._

class ConstraintSolvingChoice[S]
( val k : Logged[S] => Unit,
  var remainingChoices : Set[S],
  var triedChoices : Set[S]
  ) extends StateCreator[DependencyGraph, ConstraintSolvingChoice[S]]{

  def createState(givenId : Int,
                  previousState : Option[SearchState[DependencyGraph]],
                  currentResult : Logged[DependencyGraph],
                  choices : ConstraintSolvingChoice[S]) : SearchState[DependencyGraph] =
    new ConstraintSolvingState[S]{
      val id = givenId
      val loggedResult = currentResult
      val internal = choices
      val prevState = previousState
    }
}

trait ConstraintSolvingState[S]
  extends SearchState[DependencyGraph]{

  /*println("creating searchState "+ id)
  prevState match {
    case None => ()
    case Some(p) =>  println("parent is " + p.uuid())
  }*/

  val internal : ConstraintSolvingChoice[S]
  import internal._

  def triedAll = remainingChoices.isEmpty

  override def isMarkPointState = {
    recordOfResult(loggedResult.value).nonEmpty && (prevState forall { s =>
      recordOfResult(s.loggedResult.value).size < recordOfResult(loggedResult.value).size
    })
  }

  override def executeNextChoice(engine : SearchEngine[DependencyGraph]) : Unit = {
    if(remainingChoices.nonEmpty){

      val c = remainingChoices.head
      remainingChoices -= c
      triedChoices += c

      k(c.set(loggedResult.written))

    }
  }


}
