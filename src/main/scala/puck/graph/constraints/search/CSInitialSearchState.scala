package puck.graph.constraints.search

import puck.graph._
import puck.graph.constraints.Solver
import puck.search.{SearchEngine, SearchState}

class CSInitialSearchState(solver : Solver,
                           graph : DependencyGraph,
                           k : Try[ResultT] => Unit)
  extends SearchState[ResultT]{

  val id: Int = 0
  val prevState: Option[SearchState[ResultT]] = None
  val result = (graph, graph.recording)
  var executedOnce = false
  override def triedAll = executedOnce

  override def executeNextChoice(engine : SearchEngine[ResultT]) : Unit = {
    solver.solve(graph, k)
    executedOnce = true
  }



}
