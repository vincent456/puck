package puck.graph.constraints

import puck.graph.backTrack.{SearchStateBreakPoint, Recording}
import puck.graph._
import scala.collection.mutable

/**
 * Created by lorilan on 01/07/14.
 */


class SearchStateIterator (val root : SearchState)
  extends BreadthFirstTreeIterator[SearchState]

class SearchState(val id : Int,
                  val engine : SearchEngine,
                  val recording: Recording,
                  val k: Option[AGNode] => Unit,
                  val remainingChoices : mutable.Set[AGNode],
                  val prevState : Option[SearchState])
extends HasChildren[SearchState]{

  def children = nextStates

  def iterator = new SearchStateIterator(this)

  println("creating searchState "+ id)
  prevState match {
    case None => ()
    case Some(p) =>  println("parent is " + p.uuid())
  }

  val triedChoices = mutable.Set[AGNode]()

  val nextStates = mutable.ListBuffer[SearchState]()

  var cid = -1

  var isStep = false

  private def uuid0 : List[(Int, Boolean)] = {
    prevState match{
      case None => List((id, isStep))
      case Some(parent) => (id, isStep) :: parent.uuid0
    }
  }

  def uuid : List[(Int, Boolean)] = {
    this.uuid0.reverse
  }


  def uuid(printPointSep : String = "/",
           normalSep : String = "_",
           end : String = "") : String = {
    val sb = new StringBuilder()

    def aux(uuid: List[(Int, Boolean)]): Unit = uuid match {
      case List() => throw new Error("invalid uuid !!")
      case (i, _) :: List() =>
        sb.append(i)
        sb.append(end)
      case (i, printPoint) :: tl =>
        sb.append(i)
        sb.append(if (printPoint) printPointSep
        else normalSep)
        aux(tl)
    }

    aux(uuid)
    sb.toString()
  }

  def depth : Int = {
    def aux(sstate : Option[SearchState], acc : Int) : Int = sstate match {
      case None => acc
      case Some(s) => aux(s.prevState,
        if(s.isStep) acc + 1
        else acc)
    }
    aux(prevState, 0)
  }


  def nextChildId() : Int = {
    cid += 1
    cid
  }

  def setState(){
    val r = engine.graph.transformations.recording
    r.undo()
    recording.redo()
    engine.graph.transformations.recording = recording
    engine.currentState = this
  }

  val needToTryNone = remainingChoices.isEmpty
  var triedNone = false

  def triedAll =
    (!needToTryNone && remainingChoices.isEmpty) ||
      (needToTryNone && triedNone)

  def executeNextChoice(){
    if(engine.currentState != this)
      setState()

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
          engine.graph.transformations.undo(SearchStateBreakPoint())
          executeNextChoice()
      }

    }
    if(needToTryNone){
      triedNone = true
      k(None)
    }
  }

}




trait SearchEngine extends DecisionMaker{


  val solver : Solver
  lazy val graph : AccessGraph = solver.graph
  val printTrace : SearchState => Unit
  val violationsKindPriority : List[NodeKind]

  lazy val initialState = new SearchState(0, this, graph.transformations.recording,
           {case _ => solver.solve(() => printTrace(currentState))},
           mutable.Set(), None)

  var currentState : SearchState = _

  override def toString = "Default Strategy"

  def start(){
    currentState = initialState
    currentState.executeNextChoice()
  }

  def keepGoing() = {
    currentState.executeNextChoice()
  }

  def newCurrentState(k: Option[AGNode] => Unit,
               it : Iterator[AGNode]) {

    //only one choice : no state created
    /*if(it.isEmpty)
      k(None)
    else {
      val remainingChoices = mutable.Set[AGNode]() ++ it

      if (remainingChoices.size == 1)
        k(Some(remainingChoices.head))
      else {*/
      val remainingChoices = mutable.Set[AGNode]() ++ it
        val newState = new SearchState(currentState.nextChildId(), this,
          graph.transformations.recording, k,
          remainingChoices, Some(currentState))
        currentState.nextStates += newState
        currentState = newState
        keepGoing()
    /*  }
    }*/
  }


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
    newCurrentState(k, aux(violationsKindPriority))
  }


  def abstractionKindAndPolicy(impl : AGNode) = {
    val policy = impl.kind.abstractionPolicies.head
    (impl.kind.abstractKinds(policy).head, policy)
  }

  def chooseNode(context : => String,
                 predicate : AGNode => Boolean,
                 k : Option[AGNode] => Unit) {
    println(context)
    newCurrentState(k, graph.filter(predicate))
  }

  def modifyConstraints(sources : NodeSet, target : AGNode){}

}
