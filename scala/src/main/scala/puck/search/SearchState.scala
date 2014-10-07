package puck.search

import puck.util.{HasChildren, BreadthFirstTreeIterator}

import scala.collection.mutable

/**
 * Created by lorilan on 22/07/14.
 */
class SearchStateIterator[R](val root : SearchState[R, _])
  extends BreadthFirstTreeIterator[SearchState[R, _]]

trait StateCreator[Result, Internal]{
  def createState(id : Int,
                  engine : SearchEngine[Result],
                  prevState : Option[SearchState[Result, _]],
                  currentResult : Result,
                  choices : Internal) : SearchState[Result, Internal]
}

trait SearchState[Result, Internal] extends HasChildren[SearchState[Result, _]]{

  val result : Result
  val internal : Internal
  val id : Int
  val engine : SearchEngine[Result]
  val prevState : Option[SearchState[Result, _]]

  def createNextState[S <: StateCreator[Result, S]](cr : Result, choices : S) : SearchState[Result, S] = {
    val s = choices.createState(this.nextChildId(), this.engine, Some(this), cr, choices)
    this.nextStates += s
    s
  }

  override def toString = uuid()

  def children = nextStates

  def iterator = new SearchStateIterator(this)



  val nextStates = mutable.ListBuffer[SearchState[Result, _]]()

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
    def aux(sstate : Option[SearchState[Result, _]], acc : Int) : Int = sstate match {
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
