package puck.graph.constraints

import puck.graph.backTrack.{SearchStateBreakPoint, Recording}
import puck.graph._
import puck.search.{SearchEngine, TryAllSearchEngine, SearchState}
import puck.util.Logger

import scala.collection.mutable

/**
 * Created by lorilan on 01/07/14.
 */


class ConstraintSolvingChoices(val recording : Recording,
                               val remainingChoices : mutable.Set[AGNode],
                               val triedChoices : mutable.Set[AGNode])

class ConstraintSolvingSearchState(val id : Int,
                                   val engine : SearchEngine[ConstraintSolvingChoices, Option[AGNode]],
                                   val internal: ConstraintSolvingChoices,
                                   val k: Option[AGNode] => Unit,
                                   val prevState : Option[SearchState[ConstraintSolvingChoices, Option[AGNode]]])
  extends SearchState[ConstraintSolvingChoices, Option[AGNode]]{

  println("creating searchState "+ id)
  prevState match {
    case None => ()
    case Some(p) =>  println("parent is " + p.uuid())
  }

  def createState(id : Int,
                  engine : SearchEngine[ConstraintSolvingChoices, Option[AGNode]],
                  k: Option[AGNode] => Unit,
                  prevState : Option[SearchState[ConstraintSolvingChoices, Option[AGNode]]],
                  hook : ConstraintSolvingChoices) : ConstraintSolvingSearchState = {
    new ConstraintSolvingSearchState(id, engine, hook, k, prevState)
  }

  import internal._

  override def setAsCurrentState(){
    recording()
    super.setAsCurrentState()
  }

  val needToTryNone = internal.remainingChoices.isEmpty
  var triedNone = false

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
          println("state %s, choice %s aborted :\n %s".format(uuid(), c, e.getMessage))
          recording.graph.transformations.undo(SearchStateBreakPoint())
          executeNextChoice()
      }

    }
    if(needToTryNone){
      triedNone = true
      k(None)
    }
  }

}

class CSInitialSearchState(e : SearchEngine[ConstraintSolvingChoices, Option[AGNode]],
                           solver : Solver,
                           printTrace : SearchState[ConstraintSolvingChoices, Option[AGNode]] => Unit)
  extends ConstraintSolvingSearchState(0, e, new ConstraintSolvingChoices(solver.graph.transformations.recording,
    mutable.Set(), mutable.Set()), null, None){

  override val triedAll = true
  override def executeNextChoice(){
    solver.solve(() => printTrace(e.currentState))
  }
}


trait ConstraintSolvingSearchEngine
  extends TryAllSearchEngine[ConstraintSolvingChoices,
    Option[AGNode]] with DecisionMaker{

  val logger : Logger

  val violationsKindPriority : List[NodeKind]

  override def toString = "Try all Strategy"

  def violationTarget(k: Option[AGNode] => Unit) {
    def aux : List[NodeKind] => Iterator[AGNode] =  {
      case topPriority :: tl =>
        val it = graph.filter{ n =>
          n.kind == topPriority && (n.wrongUsers.nonEmpty ||
            n.isWronglyContained)
        }
        if(it.hasNext) it
        else aux(tl)

      case List() => graph.filter{ n => n.wrongUsers.nonEmpty ||
        n.isWronglyContained }
    }
    val choices = new ConstraintSolvingChoices(graph.transformations.recording,
      mutable.Set[AGNode]() ++ aux(violationsKindPriority),
      mutable.Set[AGNode]())

    if(choices.remainingChoices.nonEmpty)
      newCurrentState(k, choices)
    else {
      val fs = currentState.createNextState(k, choices)
      finalStates.find( s =>
          s.internal.recording.produceSameGraph(fs.internal.recording)) match{
        case Some(s) =>
          logger.writeln("final states %s and %s have same graph".format(s.uuid(), currentState.uuid()))
        case None =>
          logger.writeln("final state found : " + fs.uuid() )
          finalStates += fs
          fs.executeNextChoice() // this will call step one last time
      }

    }
  }

  def abstractionKindAndPolicy(impl : AGNode) = {
    val policy = impl.kind.abstractionPolicies.head
    (impl.kind.abstractKinds(policy).head, policy)
  }

  def chooseNode(context : => String,
                 predicate : AGNode => Boolean,
                 k : Option[AGNode] => Unit) {
    println(context)
    newCurrentState(k, new ConstraintSolvingChoices(graph.transformations.recording,
      mutable.Set[AGNode]() ++ graph.filter(predicate),
      mutable.Set[AGNode]()))
  }

  def modifyConstraints(sources : NodeSet, target : AGNode){}

  override def keepGoing(){
    if(currentState.internal.recording.isEmpty)
      super.keepGoing()
    else
      initialState.iterator.find(s =>
        !(s eq currentState) &&
          s.internal.recording.produceSameGraph(currentState.internal.recording)
      ) match {
        case Some(s) =>
          logger.writeln("states %s and %s have same graph".format(s.uuid(), currentState.uuid()))
        case None => super.keepGoing()
      }
  }

}
