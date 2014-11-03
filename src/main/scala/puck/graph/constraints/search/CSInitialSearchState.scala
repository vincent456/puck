package puck.graph.constraints.search

import puck.graph._
import puck.graph.constraints.{DecisionMaker, Solver}
import puck.search.SearchEngine

import scala.collection.mutable

trait InitialStateCreator[Kind <: NodeKind[Kind], T] {
  this : SearchEngine[ResultT[Kind, T]] with  DecisionMaker[Kind, T] =>

  val graph : AccessGraph[Kind, T]
  val solverBuilder : SolverBuilder[Kind, T]

  def logger = graph.logger
  def initialState = new CSInitialSearchState(this, solverBuilder(graph, this), graph)

}

/**
 * Created by lorilan on 25/10/14.
 */
class CSInitialSearchState[Kind <: NodeKind[Kind], T](e : SearchEngine[ResultT[Kind, T]],
                                                   solver : Solver[Kind, T],
                                                   graph : AccessGraph[Kind, T])
  extends ConstraintSolvingNodeChoiceSearchState[Kind, T](0, (graph, graph.recording), e,
    new ConstraintSolvingNodesChoice[Kind, T](null, mutable.Set(), mutable.Set()), None){

  var executedOnce = false
  override def triedAll = executedOnce

  override def executeNextChoice(){
    //solver.solve(() => printTrace(e.currentState))
    solver.solve(graph)
    executedOnce = true
  }
}
