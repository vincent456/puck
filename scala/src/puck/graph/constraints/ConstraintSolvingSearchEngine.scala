package puck.graph.constraints

import puck.graph.backTrack.{SearchStateBreakPoint, Recording}
import puck.graph._
import puck.search.{GradedSearchEngine, SearchEngine, TryAllSearchEngine, SearchState}
import puck.util.Logger

import scala.collection.mutable

/**
 * Created by lorilan on 01/07/14.
 */


class ConstraintSolvingChoices[Kind <: NodeKind[Kind]](val recording : Recording[Kind],
                               val remainingChoices : mutable.Set[AGNode[Kind]],
                               val triedChoices : mutable.Set[AGNode[Kind]])

class ConstraintSolvingSearchState[Kind <: NodeKind[Kind]](val id : Int,
                                   val engine : SearchEngine[ConstraintSolvingChoices[Kind], Option[AGNode[Kind]]],
                                   val internal: ConstraintSolvingChoices[Kind],
                                   val k: Option[AGNode[Kind]] => Unit,
                                   val prevState : Option[SearchState[ConstraintSolvingChoices[Kind], Option[AGNode[Kind]]]])
  extends SearchState[ConstraintSolvingChoices[Kind], Option[AGNode[Kind]]]{

  println("creating searchState "+ id)
  prevState match {
    case None => ()
    case Some(p) =>  println("parent is " + p.uuid())
  }

  def createState(id : Int,
                  engine : SearchEngine[ConstraintSolvingChoices[Kind], Option[AGNode[Kind]]],
                  k: Option[AGNode[Kind]] => Unit,
                  prevState : Option[SearchState[ConstraintSolvingChoices[Kind], Option[AGNode[Kind]]]],
                  hook : ConstraintSolvingChoices[Kind]) : ConstraintSolvingSearchState[Kind] = {
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

class CSInitialSearchState[Kind <: NodeKind[Kind]](e : SearchEngine[ConstraintSolvingChoices[Kind], Option[AGNode[Kind]]],
                           solver : Solver[Kind],
                           printTrace : SearchState[ConstraintSolvingChoices[Kind], Option[AGNode[Kind]]] => Unit)
  extends ConstraintSolvingSearchState[Kind](0, e, new ConstraintSolvingChoices(solver.graph.transformations.recording,
    mutable.Set(), mutable.Set()), null, None){

  override val triedAll = true
  override def executeNextChoice(){
    solver.solve(() => printTrace(e.currentState))
  }
}


trait ConstraintSolvingSearchEngineDecisionMaker[Kind <: NodeKind[Kind]]
  extends SearchEngine[ConstraintSolvingChoices[Kind],
    Option[AGNode[Kind]]] with DecisionMaker[Kind]{

  val logger : Logger[Int]

  val violationsKindPriority : List[Kind]

  override def toString = "Try all Strategy"

  def violationTarget(k: Option[NodeType] => Unit) {
    def aux : List[Kind] => Iterator[NodeType] =  {
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

    val violationTargets = new ConstraintSolvingChoices(graph.transformations.recording,
      mutable.Set[NodeType]() ++ aux(violationsKindPriority),
      mutable.Set[NodeType]())

    if(violationTargets.remainingChoices.nonEmpty)
      newCurrentState(k, violationTargets)
    else {
      val fs = currentState.createNextState(k, violationTargets)
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

  def abstractionKindAndPolicy(impl : NodeType) = {
    val policy = impl.kind.abstractionPolicies.head
    (impl.kind.abstractKinds(policy).head, policy)
  }

  def chooseNode(context : => String,
                 predicate : NodeType => Boolean,
                 k : Option[NodeType] => Unit) {
    //println(context)
    newCurrentState(k, new ConstraintSolvingChoices(graph.transformations.recording,
      mutable.Set[NodeType]() ++ graph.filter(predicate),
      mutable.Set[NodeType]()))
  }

  def modifyConstraints(sources : NodeSet[Kind], target : NodeType){}

}


trait TryAllConstraintSolvingSearchEngine[Kind <: NodeKind[Kind]]
  extends TryAllSearchEngine[ConstraintSolvingChoices[Kind],
    Option[AGNode[Kind]]] with ConstraintSolvingSearchEngineDecisionMaker[Kind]{

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


trait GradedConstraintSolvingSearchEngine[Kind <: NodeKind[Kind]]
  extends GradedSearchEngine[ConstraintSolvingChoices[Kind],
    Option[AGNode[Kind]]] with ConstraintSolvingSearchEngineDecisionMaker[Kind]{

  override def keepGoing(){}
}
