package puck.search

import scala.collection.mutable

/**
 * Created by lorilan on 26/10/14.
 */

trait Evaluator[Result]{
  
  type StateT = SearchState[Result]

  def filterDifferentStates (l : Seq[StateT]) : Seq[StateT] = {
    def aux(l : Seq[StateT], acc : Seq[StateT]) : Seq[StateT] = {
      if (l.nonEmpty) {
        aux(l.tail,
          if (!l.tail.exists { st => equals(st, l.head)})
            l.head +: acc
          else acc)
      }
      else acc
    }
    aux(l, Seq())
  }

  def filterDifferentStates (m : Map[Int, Seq[StateT]]) : Map[Int, Seq[StateT]] =
    m.foldLeft(Map[Int, Seq[StateT]]()){
      case (acc, (k, v)) => acc + (k -> filterDifferentStates(v))
    }

  def sort(l : Seq[StateT], pres : Int = 2)={
    def aux(acc : Map[Int, Seq[StateT]], l : Seq[StateT]) :
    Map[Int, Seq[StateT]] =
      if(l.isEmpty) acc
      else{
        l.head.setAsCurrentState()
        val presDix = Math.pow(10, pres.toDouble)
        val value = (evaluate(l.head) * presDix).toInt
        val olds = acc.getOrElse(value, Seq())
        aux(acc + (value -> (l.head +: olds)), l.tail)
      }
    aux(Map[Int, Seq[StateT]](), l)
  }

  def evaluate(r : StateT) : Double
  def equals(r1 : StateT, r2 : StateT) : Boolean
  
}

trait FunneledSeachEngine[Result] extends SearchEngine[Result]{

  def evaluator : Evaluator[Result]
  var stateStack = new mutable.Stack[SearchState[Result]]()

  lazy val funneledStates = new FunneledStatesStack[Result](evaluator)

  def markPointPeriod : Int = 3

  var currentCheckPoint : Int = markPointPeriod

  override def init(k : Try[Result] => Unit) : Unit = {
    //println("StackedSearchEngine.init")
    super.init(k)
    stateStack.push(initialState)
    numExploredStates = 1
  }

  override def newCurrentState[S <: StateCreator[Result, S]](cr : Result, choices : S) : Unit =  {
    //println("StackedSearchEngine.newCurrentState")
    super.newCurrentState(cr, choices)
    if ( currentState.markedPointDepth >= currentCheckPoint)
      funneledStates push currentState
    else
      stateStack push currentState
    ()
  }




  override def doExplore(k : Try[Result] => Unit) = {

     this.search(k)


      while (stateStack.nonEmpty) {

        while (stateStack.nonEmpty) {
          if (stateStack.head.triedAll)
            stateStack.pop()
          else {
              stateStack.head.executeNextChoice()
          }
        }

        val freezedStates = funneledStates.produce()

        stateStack.clear()
        stateStack pushAll freezedStates
        funneledStates.clear()

        currentCheckPoint += markPointPeriod
      }
  }
}

/*
trait FunneledSeachEngine[Result] extends StackedSearchEngine[Result]{

  val evaluator : Evaluator[Result]


  val funneledStates = new FunneledStates(30, evaluator)

  def markPointPeriod : Int = 5

  var currentMileStone = markPointPeriod

  override def doExplore( k : Try[Result] => Unit) {
    this.search(k)

    def searchIteration(){
      println(exploredStates)

      stateStack pushAll funneledStates()
      funneledStates.clear()
      currentMileStone += markPointPeriod

      while (stateStack.nonEmpty) {
        if (stateStack.head.triedAll)
          stateStack.pop()
        else if (stateStack.head.markedPointDepth == currentMileStone)
          funneledStates += stateStack.pop()
        else {
          stateStack.head.executeNextChoice()
        }
      }
      if(funneledStates().nonEmpty)
        searchIteration()
    }

    searchIteration()
  }


}*/

class FunneledStatesStack[Result](val evaluator : Evaluator[Result]) {

  val sizeMax: Int = 30
/*  private[this] val content = mutable.Stack[SearchState[Result]]()

  def produce(): Seq[SearchState[Result]] = content

  def clear() {
    content.clear()
  }

  def push(s: SearchState[Result]): Unit = {
    content.push(s)
  }*/


  private [this] var content = Map[Int, Seq[SearchState[Result]]]()

  def produce() : Seq[SearchState[Result]] = {
    content = evaluator.filterDifferentStates(content)
    val values = content.keys.toSeq.sorted.reverse

    val (res, _) = values.foldLeft((Seq[SearchState[Result]](), 0)){
      case ((acc, size), k) =>
        if(size>=sizeMax) (acc, size)
        else{
          val vs = content(k)
          if(vs.size + size > sizeMax)
            (vs.slice(0, sizeMax - size) ++: acc, sizeMax)
          else (vs ++: acc, size)
        }
    }

    res
  }

  def clear() = {
    content = Map[Int, Seq[SearchState[Result]]]()
  }


  def push(s : SearchState[Result]): Unit = {
    val value = (evaluator.evaluate(s) * 100).toInt
    val olds = content.getOrElse(value, Seq())
    content =  content + (value -> (s +: olds))
  }

}

