package puck.graph.constraints.search

import puck.graph.constraints.Solver
import puck.graph.constraints.search.CSInitialSearchState.CSInitialStateFactory
import puck.graph.{DependencyGraph, NodeKind, ResultT}
import puck.graph.transformations.{TransformationRules, Transformation}
import puck.search._

class ConstraintSolvingSearchEngineBuilder
( val violationsKindPriority : Seq[NodeKind],
  val rules : TransformationRules,
  var searchStrategy: SearchStrategy[ResultT],
  var initialStateFactory : CSInitialStateFactory) {
  def apply
  ( initialRecord : Seq[Transformation],
    graph : DependencyGraph,
    automaticConstraintLoosening : Boolean) :
    SearchEngine[ResultT] = {
    val dm = new ConstraintSolvingSearchEngineDecisionMaker(violationsKindPriority)

    val solver = new Solver(dm, rules, automaticConstraintLoosening)

    val searchEngine =
      new SearchEngine[ResultT](initialStateFactory(solver, graph), searchStrategy)

    dm.searchEngine = searchEngine

    searchEngine
  }
}