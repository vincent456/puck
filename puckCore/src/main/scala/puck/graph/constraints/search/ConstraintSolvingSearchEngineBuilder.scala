package puck.graph.constraints.search

import puck.graph.constraints.Solver
import puck.graph.constraints.search.CSInitialSearchState.CSInitialStateFactory
import puck.graph.{GraphUtils, DependencyGraph, ResultT}
import puck.search._

class ConstraintSolvingSearchEngineBuilder
( val graphUtils: GraphUtils,
  var searchStrategy: SearchStrategy[ResultT],
  var initialStateFactory : CSInitialStateFactory) {

  def apply
  ( graph : DependencyGraph,
    automaticConstraintLoosening : Boolean) :
    SearchEngine[ResultT] = {
    val dm = new ConstraintSolvingSearchEngineDecisionMaker(graphUtils.violationsKindPriority)

    val solver = new Solver(dm, graphUtils.transformationRules, automaticConstraintLoosening)

    val searchEngine =
      new SearchEngine[ResultT](initialStateFactory(solver, graph), searchStrategy)

    dm.searchEngine = searchEngine

    searchEngine
  }
}