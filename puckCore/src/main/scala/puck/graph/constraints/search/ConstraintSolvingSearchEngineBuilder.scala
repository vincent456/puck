/*
package puck.graph.constraints.search

import puck.graph.constraints.Solver
import puck.graph.constraints.search.CSInitialSearchState.CSInitialStateFactory
import puck.graph.{GraphUtils, DependencyGraph}
import puck.search._

class ConstraintSolvingSearchEngineBuilder
( val graphUtils: GraphUtils,
  var searchStrategy: SearchStrategy[DependencyGraph],
  var initialStateFactory : CSInitialStateFactory) {

  def apply
  ( graph : DependencyGraph,
    automaticConstraintLoosening : Boolean) :
    SearchEngine[DependencyGraph] = {
    val dm = new ConstraintSolvingSearchEngineDecisionMaker(graphUtils.violationsKindPriority)

    val solver = new Solver(dm, graphUtils.transformationRules, automaticConstraintLoosening)

    val searchEngine =
      new SearchEngine[DependencyGraph](initialStateFactory(solver, graph), searchStrategy)

    dm.searchEngine = searchEngine

    searchEngine
  }
}*/
