package puck.graph.constraints.search

import puck.graph.{RedirectionError, NodeKind}
import puck.graph.backTrack.Recording
import puck.search.{StateCreator, SearchState}
import puck.util.PuckLog

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

  private val needToTryNone = true//internal.remainingChoices.isEmpty
  private var triedNone = false

  def triedAll =
    (!needToTryNone && internal.remainingChoices.isEmpty) ||
      (needToTryNone && triedNone)

  def executeNextChoice(){
    if(engine.currentState != this)
      setAsCurrentState()

    if(remainingChoices.nonEmpty){
      val c = remainingChoices.head
      remainingChoices.remove(c)
      triedChoices.add(c)

      val breakPoint = result.graph.transformations.startSequence()
      try {
        k(Some(c))
      }catch{
        case e : RedirectionError =>
          result.graph.logger.writeln(("state %s, redirection error catched, " +
            "choice %s aborted :\n %s").format(uuid(), c, e.getMessage))(PuckLog.Search(), PuckLog.Info())
          result.graph.transformations.undo(breakPoint)
          executeNextChoice()
      }

    }
    else if(needToTryNone){
      triedNone = true
      k(None)
    }
  }


}
