package puck.search

import puck.graph.{LoggedSuccess, LoggedTry}

import scalaz.{\/-, -\/}

class IntStrategy
(strategy : SearchStrategy[Int],
 target : Int
  ) extends SearchStrategyDecorator[Int](strategy){

  import IntSearch._

  override def oneStep: Option[(LoggedTry[Int],
    Seq[LoggedTry[Int]])] = {
    strategy.nextState.nextChoice flatMap( lt =>
      lt.value match {
        case \/-(i) if i == target => Some((lt, Seq()))
        case \/-(i) => Some((lt, actionPlus(i) ++ actionMoins(i)))
        case -\/(_) => None
      })
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

  val control = new IntStrategy(
    new BreadthFirstSearchStrategy(),
    //new DepthFirstSearchStrategy(),
    5)
  val se = new SearchEngine[Int](IntSearch.initialState(0),
    control,
    Some(1))
  println("launching search ... ")
  se.explore()
  println("Explored States : " + se.exploredStates)
  println("Success depth : " + se.successes.head.depth)
}
