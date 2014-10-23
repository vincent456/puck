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


  def uuid(sep : String = "_") : String = uuid.mkString(sep)

/*  def uuid(printPointSep : String = "/",
           normalSep : String = "_",
           end : String = "") : String = {
    val sb = new StringBuilder()

    def aux(uuid: Seq[(Int, Boolean)]): Unit = uuid match {
      case Seq() => throw new Error("invalid uuid !!")
      case (i, _) +: Seq() =>
        sb.append(i)
        sb.append(end)
      case (i, printPoint) +: tl =>
        sb.append(i)
        sb.append(if (printPoint) printPointSep
        else normalSep)
        aux(tl)
    }

    aux(uuid)
    sb.toString()
  }*/

  /*def depth : Int = {
    def aux(sstate : Option[SearchState[Result, _]], acc : Int) : Int = sstate match {
      case None => acc
      case Some(s) => aux(s.prevState,
        if(s.isStep) acc + 1
        else acc)
    }
    aux(prevState, 0)
  }*/

  def depth : Int = uuid0.size

  def nextChildId() : Int = {
    cid += 1
    cid
  }

  def setAsCurrentState(){ engine.currentState = this }

  def triedAll : Boolean

  def executeNextChoice() : Unit

  def ancestors(includeSelf : Boolean) :List[SearchState[Result, _]] = {
    def aux(sState : Option[SearchState[Result,_]],
            acc : List[SearchState[Result,_ ]]) : List[SearchState[Result,_ ]] =
    sState match {
      case None => acc
      case Some(state) => aux(state.prevState, state :: acc)
    }

    aux(if(includeSelf) Some(this) else this.prevState, List())

  }

}
