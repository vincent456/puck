package puck.graph.constraints.search

import puck.graph.constraints.Solver
import puck.graph.constraints.search.CSInitialSearchState.CSInitialStateFactory
import puck.graph.{DependencyGraph, NodeKind, ResultT}
import puck.graph.transformations.{TransformationRules, Transformation}
import puck.search.{TryAllSearchEngine, FunneledSeachEngine, FindFirstSearchEngine, SearchEngine}
import puck.search.SearchEngine.InitialStateFactory

object ConstraintSolvingSearchEngineBuilder {
  type InitialRecord = Seq[Transformation]
  type SearchEngineBuilder = ( InitialRecord , InitialStateFactory[ResultT]) => SearchEngine[ResultT]

  object FindFirstCSSEBuilder
    extends Function2[InitialRecord, InitialStateFactory[ResultT], SearchEngine[ResultT]]{
    override def toString() = "First solution"
    def apply
    ( initialRecord : Seq[Transformation],
      initialStateFactory : InitialStateFactory[ResultT]) =
      new FindFirstSearchEngine[ResultT](initialStateFactory)
  }

  object FunneledCSSEBuilder
    extends Function2[InitialRecord, InitialStateFactory[ResultT], SearchEngine[ResultT]]{
    override def toString() = "Funneled"
    def apply
    ( initialRecord : Seq[Transformation],
      initialStateFactory : InitialStateFactory[ResultT]) =
      new FunneledSeachEngine[ResultT](initialStateFactory,
        new ConstraintSolvingStateEvaluator(initialRecord))
  }

  object TryAllCSSEBuilder
    extends Function2[InitialRecord, InitialStateFactory[ResultT], SearchEngine[ResultT]]{
    override def toString() = "Try all"
    def apply
    ( initialRecord : Seq[Transformation],
      initialStateFactory : InitialStateFactory[ResultT]) =
      new TryAllSearchEngine[ResultT](initialStateFactory)
  }

  val searchingStrategies : Seq[SearchEngineBuilder] =
    Seq(FindFirstCSSEBuilder, FunneledCSSEBuilder, TryAllCSSEBuilder)
}

import ConstraintSolvingSearchEngineBuilder._
class ConstraintSolvingSearchEngineBuilder
( val violationsKindPriority : Seq[NodeKind],
  val rules : TransformationRules,
  var searchEngineBuilder: SearchEngineBuilder,
  var initialStateFactory : CSInitialStateFactory) {
  def apply
  ( initialRecord : Seq[Transformation],
    graph : DependencyGraph,
    automaticConstraintLoosening : Boolean) :
    SearchEngine[ResultT] = {
    val dm = new ConstraintSolvingSearchEngineDecisionMaker(violationsKindPriority)

    val solver = new Solver(dm, rules, automaticConstraintLoosening)

    val searchEngine = searchEngineBuilder(initialRecord, initialStateFactory(solver, graph))

    dm.searchEngine = searchEngine

    searchEngine
  }
}