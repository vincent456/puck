package puck.graph.constraints.search

import puck.graph._
import puck.graph.constraints.Solver
import puck.graph.constraints.search.CSInitialSearchState.Starter
import puck.search.{SearchEngine, SearchState}, SearchEngine.InitialStateFactory

import scalaz._, Scalaz._

object CSInitialSearchState {
  type Starter = (Solver, DependencyGraph, LoggedTry[ResultT] => Unit) => Unit

  type CSInitialStateFactory =
    (Solver, DependencyGraph) => InitialStateFactory[ResultT]

  val default : Starter =
    (solver, graph, k) => solver.solve(graph, k)

  def targeted(n : ConcreteNode) : Starter =
    (solver, graph, k) => solver.solveViolationsToward(graph.set(""),n)(k)


  val defaultInitialState : CSInitialStateFactory =
    (solver, graph) =>
      k => new CSInitialSearchState(solver, graph, k, default)

  def targetedInitialState(n : ConcreteNode) : CSInitialStateFactory =
    (solver, graph) =>
      k => new CSInitialSearchState(solver, graph, k, targeted(n))

}

class CSInitialSearchState(solver : Solver,
                           graph : DependencyGraph,
                           k : LoggedTry[ResultT] => Unit,
                           starter : Starter)
  extends SearchState[ResultT]{

  val id: Int = 0
  val prevState: Option[SearchState[ResultT]] = None
  val loggedResult = graph.set("")
  var executedOnce = false
  override def triedAll = executedOnce

  override def executeNextChoice(engine : SearchEngine[ResultT]) : Unit = {
    starter(solver, graph, k)
    executedOnce = true
  }



}
