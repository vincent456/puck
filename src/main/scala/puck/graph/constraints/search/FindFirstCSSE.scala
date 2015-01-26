package puck.graph.constraints.search

import puck.graph.transformations.Recording
import puck.graph.{ResultT, DependencyGraph, NodeKind}
import puck.search.FindFirstSearchEngine

/**
 * Created by lorilan on 25/09/14.
 */
//CSSE : Constraint Solving Search Engine
class FindFirstCSSE
(val violationsKindPriority : Seq[NodeKind],
 val graph : DependencyGraph,
 val solverBuilder : SolverBuilder)
  extends ConstraintSolvingSearchEngineDecisionMaker
  with FindFirstSearchEngine[ResultT]
  with InitialStateCreator
