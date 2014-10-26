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
  def aux(sstate : Option[SearchState[Result, _]], acc : Int) : Int = sstate match {
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

  def setAsCurrentState(){ engine.currentState = this }

  def triedAll : Boolean

  def executeNextChoice() : Unit

  def ancestors(includeSelf : Boolean) :Seq[SearchState[Result, _]] = {
    def aux(sState : Option[SearchState[Result,_]],
            acc : Seq[SearchState[Result,_ ]]) : Seq[SearchState[Result,_ ]] =
    sState match {
      case None => acc
      case Some(state) => aux(state.prevState, state +: acc)
    }

    aux(if(includeSelf) Some(this) else this.prevState, Seq())

  }

}
