package puck.search

import puck.graph.{LoggedSuccess, LoggedTry}
import puck.search.SearchEngine.InitialStateFactory
import puck.util.Logged

import scalaz.Scalaz._

object IntSolver {

  var se : SearchEngine[Int] = null

  def findValue(currentValue : Int, target : Int, k : LoggedTry[Int] => Unit) : Unit =
    if(currentValue == target) k(LoggedSuccess(currentValue))
    else{

      val c = IntChoice(arg => findValue(currentValue + arg, target, k), List(+1,-1))
      se.addState(currentValue.set(""), c)
    }
}

case class IntChoice(k : Int => Unit,
                     choices : List[Int]) extends StateCreator[Int, IntChoice]{
  override def createState(id: Int,
                           prevState: Option[SearchState[Int]],
                           currentResult: Logged[Int],
                           choices: IntChoice): SearchState[Int] =
    new IntSearchState(currentResult, prevState, id, choices)
}

class IntSearchState
( val loggedResult: Logged[Int],
  val prevState: Option[SearchState[Int]],
  val id : Int,
  ic : IntChoice
 ) extends SearchState[Int]{

  var nextChoice = 0
  
  override def executeNextChoice(engine: SearchEngine[Int]): Unit = {
    val c = ic.choices(nextChoice)
    nextChoice += 1
    ic.k(c)
  }

  override def triedAll: Boolean = nextChoice >= ic.choices.length

}

object IntSearchState{
  //InitialStateFactory[Int] = (LoggedTry[Int] => Unit) => SearchState[Int]
  def initial(startPoint : Int, target : Int) : InitialStateFactory[Int] = {
    k =>
      new IntSearchState(startPoint.set(""), None, 0,
        IntChoice(IntSolver.findValue(_, target, k), List(startPoint)))
  }
}

object IntSearchTest extends App {

  IntSolver.se = new SearchEngine[Int](IntSearchState.initial(0,5),
    new BreadthFirstSearchStrategy(),
    //new DepthFirstSearchStrategy(),
    Some(1))
  println("launching search ... ")
  IntSolver.se.explore()
  println("Explored States : " + IntSolver.se.exploredStates)
  println("Success depth : " + IntSolver.se.successes.head.depth)
}
