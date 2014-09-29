package puck.search

import puck.util.{HasChildren, BreadthFirstTreeIterator}

import scala.collection.mutable

/**
 * Created by lorilan on 22/07/14.
 */
class SearchStateIterator[T <: StateCreator[T,T]](val root : SearchState[_, T])
  extends BreadthFirstTreeIterator[SearchState[_, T]]

trait StateCreator[S <: StateCreator[S,F], F <: StateCreator[F,F]]{
  def createState(id : Int,
                  engine : SearchEngine[F],
                  prevState : Option[SearchState[_, F]],
                  choices : S) : SearchState[S, F]
}

trait SearchState[S <: StateCreator[S, F],
                  F <: StateCreator[F, F]] extends HasChildren[SearchState[_, F]]{

  val internal : S
  val id : Int
  val engine : SearchEngine[F]
  val prevState : Option[SearchState[_, F]]

  def createNextState[S2 <: StateCreator[S2, F]](choices : S2) : SearchState[S2, F] = {
    val s = choices.createState(this.nextChildId(), this.engine, Some(this), choices)
    this.nextStates += s
    s
  }

  override def toString = uuid()

  def children = nextStates

  def iterator = new SearchStateIterator(this)



  val nextStates = mutable.ListBuffer[SearchState[_, F]]()

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
    def aux(sstate : Option[SearchState[_, F]], acc : Int) : Int = sstate match {
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

  def setAsCurrentState(){ engine.currentState = this }

  def triedAll : Boolean

  def executeNextChoice() : Unit

}
