/*
package puck.search


import puck.PuckError
import puck.util.Logged
import puck.util.{HasChildren, BreadthFirstTreeIterator}

import scala.collection.mutable

class SearchStateIterator[R]
( val root : SearchState[R]
  ) extends BreadthFirstTreeIterator[SearchState[R]]

trait StateCreator[Result, Internal]{
  def createState(id : Int,
                  prevState : Option[SearchState[Result]],
                  currentResult : Logged[Result],
                  choices : Internal) : SearchState[Result]
}



trait SearchState[T]
  extends HasChildren[SearchState[T]]{

  val loggedResult : Logged[T]

  def createNextState[S <: StateCreator[T, S]](cr : Logged[T], choices : S) : SearchState[T] = {
    val s = choices.createState(this.nextChildId(), Some(this), cr, choices)
    this.nextStates += s
    s
  }


  val id : Int
  val prevState : Option[SearchState[T]]

  private def uuid0 : Seq[Int] = {
    prevState match{
      case None => Seq(id)
      case Some(parent) => id +: parent.uuid0
    }
  }

  def uuid : Seq[Int] = {
    this.uuid0.reverse
  }

  def depth : Int = uuid0.size

  def isMarkPointState = true

  def uuid(sep : String = "_",
           considerMarkPoint : Boolean = false,
           markPointSep : String = "/") : String = {

    if(!considerMarkPoint) uuid.mkString(sep)
    else ??? //todo decide if markpointSep is after or before the mark point state !
    /*
    {
      val ancestorsSeq = ancestors(includeSelf = true)
       //here the markPointSep is before the mark point state !
      val tailStrList =  ancestorsSeq.tail map {s =>
          (if(s.isMarkPointState) markPointSep
          else sep) + s.id.toString
        }
      (ancestorsSeq.head.id.toString +: tailStrList).mkString
    }*/


  }

  def markedPointDepth : Int = {
    def aux(sstate : Option[SearchState[T]], acc : Int) : Int = sstate match {
      case None => acc
      case Some(s) => aux(s.prevState,
        if(s.isMarkPointState) acc + 1
        else acc)
    }
    aux(prevState, 0)
  }

  def ancestors(includeSelf : Boolean) :Seq[SearchState[T]] = {
    def aux(sState : Option[SearchState[T]],
            acc : Seq[SearchState[T]]) : Seq[SearchState[T]] =
      sState match {
        case None => acc
        case Some(state) => aux(state.prevState, state +: acc)
      }

    aux(if(includeSelf) Some(this) else this.prevState, Seq())

  }


  override def toString = uuid()

  def children = nextStates

  def iterator = new SearchStateIterator(this)

  val nextStates = mutable.ListBuffer[SearchState[T]]()

  var cid = -1

  def nextChildId() : Int = {
    cid += 1
    cid
  }

  def triedAll : Boolean

  def executeNextChoice(engine : SearchEngine[T]) : Unit

}

class ErrorState[T]
( val id: Int,
  val result : Logged[PuckError],
  val prevState: SearchState[T]){
  def depth = prevState.depth + 1
}

class FinalState[T]
(val id: Int,
 val loggedResult : Logged[T],
 val prevState: Option[SearchState[T]])
  extends SearchState[T]{
  override def triedAll: Boolean = true
  override def executeNextChoice(engine: SearchEngine[T]): Unit = ()
}*/
