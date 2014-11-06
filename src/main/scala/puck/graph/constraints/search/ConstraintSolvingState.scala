package puck.graph.constraints.search

import puck.graph.{ResultT, NodeKind, RedirectionError, graphOfResult, recordOfResult}
import puck.search.{StateCreator, SearchState}
import puck.util.PuckLog

import scala.collection.mutable
import scala.util.Try

/**
 * Created by lorilan on 26/09/14.
 */

trait ConstraintSolvingChoice[S, T <: ConstraintSolvingChoice[S, T]]
  extends StateCreator[ResultT, T] {
  val k : Option[S] => Unit
  val remainingChoices : mutable.Set[S]
  val triedChoices : mutable.Set[S]
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


  override def setAsCurrentState(){
    recordOfResult(result)()
    super.setAsCurrentState()
  }

  private val needToTryNone0 = remainingChoices.isEmpty
  protected def needToTryNone = needToTryNone0
  private var triedNone = false

  def triedAll =
    remainingChoices.isEmpty &&
      (!needToTryNone ||  (needToTryNone && triedNone))

  override def isMarkPointState = {
    recordOfResult(result).nonEmpty && (prevState forall { s =>
      recordOfResult(s.result).size < recordOfResult(result).size
    })
  }

  def executeNextChoice(){
    if(engine.currentState != this)
      setAsCurrentState()

    if(remainingChoices.nonEmpty){
   /*if(remainingChoices.nonEmpty
      && !needToTryNone) {*/
      val c = remainingChoices.head
      remainingChoices.remove(c)
      triedChoices.add(c)

      val graph = graphOfResult(result)

      k(Some(c))


/*
      val breakPoint = graph.startSequence()
      try {
        k(Some(c))
      }catch{
        case e : RedirectionError =>
          graph.logger.writeln(("redirection error catched, " +
            "choice %s aborted :\n %s").format(c, e.getMessage))(PuckLog.Search, PuckLog.Info)
          graph.undo(breakPoint)
          executeNextChoice(end)
      }
*/

    }
    else if(needToTryNone){
      triedNone = true
      k(None)
    }
  }


}
