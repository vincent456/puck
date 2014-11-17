package puck.graph.constraints.search

import puck.graph.{ResultT, recordOfResult}
import puck.search.{StateCreator, SearchState}

/**
 * Created by lorilan on 26/09/14.
 */

trait ConstraintSolvingChoice[S, T]
  extends StateCreator[ResultT, T] {
  val k : Option[S] => Unit
  var remainingChoices : Set[Option[S]]
  var triedChoices : Set[Option[S]]
}

trait ConstraintSolvingState[ S, T <: ConstraintSolvingChoice[S, T]]
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
    recordOfResult(result).nonEmpty && (prevState forall { s =>
      recordOfResult(s.result).size < recordOfResult(result).size
    })
  }

  def executeNextChoice(){
    if(engine.currentState != this)
      setAsCurrentState()

    if(remainingChoices.nonEmpty){

      val c = remainingChoices.head
      remainingChoices -= c
      triedChoices += c

      k(c)

    }
  }


}
