package puck.graph.constraints.search

import puck.graph._
import puck.graph.constraints.{DecisionMaker, Solver}
import puck.search.{SearchState, SearchEngine}

import scala.collection.mutable
import scala.util.Try

trait InitialStateCreator {
  this : SearchEngine[ResultT] with  DecisionMaker =>

  val graph : AccessGraph
  val solverBuilder : SolverBuilder

  def logger = graph.logger
  def createInitialState(k : Try[ResultT] => Unit) =
    new CSInitialSearchState(this, solverBuilder(graph, this), graph, k)

}

/**
 * Created by lorilan on 25/10/14.
 */
class CSInitialSearchState(val engine : SearchEngine[ResultT],
                           solver : Solver,
                           graph : AccessGraph,
                           k : Try[ResultT] => Unit)
  extends SearchState[ResultT]{

  val id: Int = 0
  val prevState: Option[SearchState[ResultT]] = None
  val result = (graph, graph.recording)
  var executedOnce = false
  override def triedAll = executedOnce

  override def executeNextChoice(){
    solver.solve(graph, k)
    executedOnce = true
  }



}
