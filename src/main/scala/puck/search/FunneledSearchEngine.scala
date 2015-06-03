package puck.search

import puck.graph._
import puck.search.SearchEngine.InitialStateFactory

import scala.collection.mutable

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

  def sort(l : Seq[StateT], precision : Int = 2)={
    def aux(acc : Map[Int, Seq[StateT]], l : Seq[StateT]) :
    Map[Int, Seq[StateT]] =
      if(l.isEmpty) acc
      else{
        val presDix = Math.pow(10, precision.toDouble)
        val value = (evaluate(l.head) * presDix).toInt
        val olds = acc.getOrElse(value, Seq())
        aux(acc + (value -> (l.head +: olds)), l.tail)
      }
    aux(Map[Int, Seq[StateT]](), l)
  }

  def evaluate(r : StateT) : Double
  def equals(r1 : StateT, r2 : StateT) : Boolean

  def sortedDifferentStates(l : Seq[StateT]) :  Map[Int, Seq[StateT]] =
    filterDifferentStates(sort(l))
  
}

class FunneledSeachEngine[Result]
( val createInitialState : InitialStateFactory[Result],
  val evaluator : Evaluator[Result]
  ) extends SearchEngine[Result]{


  var stateStack = new mutable.Stack[SearchState[Result]]()

  lazy val funneledStates = new FunneledStatesStack[Result](evaluator)

  def markPointPeriod : Int = 2

  var currentCheckPoint : Int = markPointPeriod

  override def init(k : LoggedTry[Result] => Unit) : Unit = {
    //println("StackedSearchEngine.init")
    super.init(k)
    stateStack.push(initialState)
    numExploredStates = 1
  }

  override def newCurrentState[S <: StateCreator[Result, S]](cr : Logged[Result], choices : S) : Unit =  {
    //println("StackedSearchEngine.newCurrentState")
    super.newCurrentState(cr, choices)
    if ( currentState.markedPointDepth >= currentCheckPoint)
      funneledStates push currentState
    else
      stateStack push currentState
    ()
  }




  override def startExplore(k : LoggedTry[Result] => Unit) = {

     this.search(k)

      var i = 0

      while (stateStack.nonEmpty) {
        println(s"search iteration $i")
        while (stateStack.nonEmpty) {
          if (stateStack.head.triedAll) {
            stateStack.pop()
            println(s"${stateStack.size} states remainings")
          }else {
              stateStack.head.executeNextChoice(this)
          }
        }

        i += 1
        println(s"preparing iteration $i ...")


        val freezedStates = funneledStates.produce()

        stateStack.clear()
        stateStack pushAll freezedStates
        funneledStates.clear()

        currentCheckPoint += markPointPeriod
      }
    println("search finished")

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

  val sizeMax: Int = 10
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

