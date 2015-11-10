package puck.search

import puck.graph.{Error, LoggedTry, error}
import puck.util.{BreadthFirstTreeIterator, HasChildren, Logged}

import scala.collection.mutable
import scalaz.{-\/, \/-}

class SearchStateIterator[R]
( val root : SearchState[R]
  ) extends BreadthFirstTreeIterator[SearchState[R]]

class SearchState[T]
( val id : Int,
  val prevState : Option[SearchState[T]],
  val loggedResult : LoggedTry[T],
  val choices : Seq[LoggedTry[T]]
 )extends HasChildren[SearchState[T]]{


  import scalaz.syntax.writer._

  def success : Logged[T] = loggedResult.value match {
      case \/-(res) => res set loggedResult.log
      case -\/(err) => error("state contains a failed result : " + err.getMessage)
  }

  def fail : Logged[Error] = loggedResult.value match {
    case -\/(err) => err set loggedResult.log
    case _ => error("state contains a success")
  }


  def createNextState(cr : LoggedTry[T], choices : Seq[LoggedTry[T]]) : SearchState[T] = {
    val s = new SearchState(this.nextChildId(), Some(this), cr, choices)
    this.nextStates += s
    s
  }


  private [this] var _nextChoice : Seq[LoggedTry[T]] = choices

  def nextChoice : Option[LoggedTry[T]] =
    if(triedAll) None
    else {
      val n = _nextChoice.head
      _nextChoice = _nextChoice.tail
      Some(n)
    }

  def triedAll : Boolean = _nextChoice.isEmpty

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


}
