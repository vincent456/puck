package puck.graph.constraints.search

import puck.graph.{ResultT, recordOfResult}
import puck.search.{SearchEngine, StateCreator, SearchState}
import puck.util.Logged

import scalaz.syntax.writer._

trait ConstraintSolvingChoice[S, T]
  extends StateCreator[ResultT, T] {
  val k : Logged[Option[S]] => Unit
  var remainingChoices : Set[Option[S]]
  var triedChoices : Set[Option[S]]
}

trait ConstraintSolvingState[S, T <: ConstraintSolvingChoice[S, T]]
  extends SearchState[ResultT]{

  /*println("creating searchState "+ id)
  prevState match {
    case None => ()
    case Some(p) =>  println("parent is " + p.uuid())
  }*/

  val internal : ConstraintSolvingChoice[S, T]
  import internal._

  def triedAll = remainingChoices.isEmpty

  override def isMarkPointState = {
    recordOfResult(loggedResult.value).nonEmpty && (prevState forall { s =>
      recordOfResult(s.loggedResult.value).size < recordOfResult(loggedResult.value).size
    })
  }

  override def executeNextChoice(engine : SearchEngine[ResultT]) : Unit = {
    if(engine.currentState != this)
      setAsCurrentState(engine)

    if(remainingChoices.nonEmpty){

      val c = remainingChoices.head
      remainingChoices -= c
      triedChoices += c

      k(c.set(loggedResult.written))

    }
  }


}