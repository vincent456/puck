package puck.graph.constraints.search

import puck.graph.{ResultT, AccessGraph, NodeKind}
import puck.search.FunneledSeachEngine

/**
 * Created by lorilan on 26/10/14.
 */
class FunneledCSSE(val violationsKindPriority : Seq[NodeKind],
                                     val graph : AccessGraph,
                                     val solverBuilder : SolverBuilder)
  extends ConstraintSolvingSearchEngineDecisionMaker
  with FunneledSeachEngine[ResultT]
  with InitialStateCreator
