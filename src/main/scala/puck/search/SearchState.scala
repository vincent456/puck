package puck.search

import puck.graph.Logged
import puck.util.{HasChildren, BreadthFirstTreeIterator}

import scala.collection.mutable

class SearchStateIterator[R](val root : SearchState[R])
  extends BreadthFirstTreeIterator[SearchState[R]]

trait StateCreator[Result, Internal]{
  def createState(id : Int,
                  prevState : Option[SearchState[Result]],
                  currentResult : Logged[Result],
                  choices : Internal) : SearchState[Result]
}

trait SearchState[ResT] extends HasChildren[SearchState[ResT]]{

  val loggedResult : Logged[ResT]
  val id : Int
  val prevState : Option[SearchState[ResT]]

  def createNextState[S <: StateCreator[ResT, S]](cr : Logged[ResT], choices : S) : SearchState[ResT] = {
    val s = choices.createState(this.nextChildId(), Some(this), cr, choices)
    this.nextStates += s
    s
  }

  override def toString = uuid()

  def children = nextStates

  def iterator = new SearchStateIterator(this)

  val nextStates = mutable.ListBuffer[SearchState[ResT]]()

  var cid = -1

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
  def aux(sstate : Option[SearchState[ResT]], acc : Int) : Int = sstate match {
    case None => acc
    case Some(s) => aux(s.prevState,
      if(s.isMarkPointState) acc + 1
      else acc)
  }
  aux(prevState, 0)
}

  
  

  def nextChildId() : Int = {
    cid += 1
    cid
  }

  def setAsCurrentState(engine : SearchEngine[ResT]): Unit = { engine.currentState = this }

  def triedAll : Boolean

  def executeNextChoice(engine : SearchEngine[ResT]) : Unit

  def ancestors(includeSelf : Boolean) :Seq[SearchState[ResT]] = {
    def aux(sState : Option[SearchState[ResT]],
            acc : Seq[SearchState[ResT]]) : Seq[SearchState[ResT]] =
    sState match {
      case None => acc
      case Some(state) => aux(state.prevState, state +: acc)
    }

    aux(if(includeSelf) Some(this) else this.prevState, Seq())

  }

}

class FinalState[T]
(val id: Int,
 val loggedResult : Logged[T],
 val engine: SearchEngine[T],
 val prevState: Option[SearchState[T]])
  extends SearchState[T]{
  override def triedAll: Boolean = true
  override def executeNextChoice(engine: SearchEngine[T]): Unit = ()
}