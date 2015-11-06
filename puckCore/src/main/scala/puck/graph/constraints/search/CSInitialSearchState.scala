package puck.graph.constraints.search

import puck.graph.constraints.search.CSInitialSearchState.Starter
import puck.graph.{ConcreteNode, DependencyGraph, LoggedTry, DependencyGraph}
import puck.graph.constraints.Solver
import puck.search.{SearchEngine, SearchState}, SearchEngine.InitialStateFactory

import scalaz._, Scalaz._

object CSInitialSearchState {
  type Starter = (Solver, DependencyGraph, LoggedTry[DependencyGraph] => Unit) => Unit

  type CSInitialStateFactory =
    (Solver, DependencyGraph) => InitialStateFactory[DependencyGraph]

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
                           k : LoggedTry[DependencyGraph] => Unit,
                           starter : Starter)
  extends SearchState[DependencyGraph]{

  val id: Int = 0
  val prevState: Option[SearchState[DependencyGraph]] = None
  val loggedResult = graph.set("")
  var executedOnce = false
  override def triedAll = executedOnce

  override def executeNextChoice(engine : SearchEngine[DependencyGraph]) : Unit = {
    starter(solver, graph, k)
    executedOnce = true
  }



}
