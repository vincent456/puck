package puck.graph.constraints.search

import puck.graph._
import puck.graph.constraints.{DecisionMaker, Solver}
import puck.search.SearchEngine

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
class CSInitialSearchState(e : SearchEngine[ResultT],
                                                   solver : Solver,
                                                   graph : AccessGraph,
                                                   k : Try[ResultT] => Unit)
  extends ConstraintSolvingNodeChoiceSearchState(0, (graph, graph.recording), e,
    new ConstraintSolvingNodesChoice(null, mutable.Set(), mutable.Set()), None){

  var executedOnce = false
  override def triedAll = executedOnce

  override def executeNextChoice(){
    //solver.solve(() => printTrace(e.currentState))
    solver.solve(graph, k)
    executedOnce = true
  }
}
