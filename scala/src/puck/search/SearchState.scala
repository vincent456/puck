package puck.search

import puck.util.{HasChildren, BreadthFirstTreeIterator}

import scala.collection.mutable

/**
 * Created by lorilan on 22/07/14.
 */
class SearchStateIterator[S, U] (val root : SearchState[S, U])
  extends BreadthFirstTreeIterator[SearchState[S, U]]

trait SearchState[S, K]  extends HasChildren[SearchState[S, K]]{


  val internal : S
  val id : Int
  val engine : SearchEngine[S, K]
  val k: K => Unit
  val prevState : Option[SearchState[S, K]]

  def createState(id : Int,
                  engine : SearchEngine[S, K],
                  k: K => Unit,
                  prevState : Option[SearchState[S, K]],
                  choices : S) : SearchState[S, K]

  def createNextState(k: K => Unit,
                      choices : S) : SearchState[S, K] = {
    val s = createState(this.nextChildId(), this.engine, k, Some(this), choices)
    this.nextStates += s
    s
  }

  def children = nextStates

  def iterator = new SearchStateIterator(this)



  val nextStates = mutable.ListBuffer[SearchState[S, K]]()

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
    def aux(sstate : Option[SearchState[S, K]], acc : Int) : Int = sstate match {
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
