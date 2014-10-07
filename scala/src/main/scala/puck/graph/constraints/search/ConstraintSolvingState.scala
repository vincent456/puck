package puck.graph.constraints.search

import puck.graph.backTrack.{Recording, SearchStateBreakPoint}
import puck.graph.{RedirectionError, NodeKind}
import puck.search.{StateCreator, SearchState}

import scala.collection.mutable

/**
 * Created by lorilan on 26/09/14.
 */

trait ConstraintSolvingChoice[Kind <: NodeKind[Kind], S, T <: ConstraintSolvingChoice[Kind, S, T]]
  extends StateCreator[Recording[Kind] , T] {
  val k : Option[S] => Unit
  val remainingChoices : mutable.Set[S]
  val triedChoices : mutable.Set[S]
}

trait ConstraintSolvingState[Kind <: NodeKind[Kind], S, T <: ConstraintSolvingChoice[Kind, S, T]]
  extends SearchState[Recording[Kind], T]{

  /*println("creating searchState "+ id)
  prevState match {
    case None => ()
    case Some(p) =>  println("parent is " + p.uuid())
  }*/

  import internal._


  override def setAsCurrentState(){
    result()
    super.setAsCurrentState()
  }

  private val needToTryNone = internal.remainingChoices.isEmpty
  private var triedNone = false

  def triedAll =
    (!needToTryNone && internal.remainingChoices.isEmpty) ||
      (needToTryNone && triedNone)

  def executeNextChoice(){
    if(engine.currentState != this)
      setAsCurrentState()

    if(remainingChoices.nonEmpty
      && !needToTryNone) {
      val c = remainingChoices.head
      remainingChoices.remove(c)
      triedChoices.add(c)
      try {
        k(Some(c))
      }catch{
        case e : RedirectionError =>
          //TODO see if need to plug the logger
          //println("state %s, choice %s aborted :\n %s".format(uuid(), c, e.getMessage))
          result.graph.transformations.undo(SearchStateBreakPoint())
          executeNextChoice()
      }

    }
    if(needToTryNone){
      triedNone = true
      k(None)
    }
  }


}
