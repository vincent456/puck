package puck.graph.constraints.search

import puck.graph.{ResultT, AccessGraph, NodeKind}
import puck.search.TryAllSearchEngine

/**
 * Created by lorilan on 25/09/14.
 */
//CSSE : Constraint Solving Search Engine
class TryAllCSSE(
  val violationsKindPriority : Seq[NodeKind],
  val graph : AccessGraph,
  val solverBuilder : SolverBuilder)
  extends ConstraintSolvingSearchEngineDecisionMaker
  with TryAllSearchEngine[ResultT]
  with InitialStateCreator
