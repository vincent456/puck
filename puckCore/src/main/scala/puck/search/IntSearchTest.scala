package puck.search

import puck.graph.{LoggedSuccess, LoggedTry}

class IntControl
(target : Int)
  extends SearchControl[Int]{

  import IntSearch._

  def initialState: SearchState[Int] = IntSearch.initialState(0)

  def nextStates(i: Int): Seq[LoggedTry[Int]] = {
    if(i == target) Seq()
    else actionPlus(i) ++ actionMoins(i)
  }
}

object IntSearch{

  val actionPlus : Int => Seq[LoggedTry[Int]] =
    i => Seq(LoggedSuccess(i + 1))

  val actionMoins : Int => Seq[LoggedTry[Int]] =
    i => Seq(LoggedSuccess(i - 1))

  def initialState(startPoint : Int) : SearchState[Int] =
      new SearchState( 0, None, LoggedSuccess(startPoint),
        actionPlus(startPoint) ++ actionMoins(startPoint))

}

object IntSearchTest extends App {

  val se = new SearchEngine[Int](new BreadthFirstSearchStrategy(),
    new IntControl(5), Some(1))
    println("launching search ... ")
    se.explore()
    println("Explored States : " + se.exploredStates)
    println("Success depth : " + se.successes.head.depth)
}
