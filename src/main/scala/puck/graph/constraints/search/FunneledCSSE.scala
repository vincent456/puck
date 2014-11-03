package puck.graph.constraints.search

import puck.graph.{ResultT, AccessGraph, NodeKind}
import puck.search.FunneledSeachEngine

/**
 * Created by lorilan on 26/10/14.
 */
class FunneledCSSE[Kind <: NodeKind[Kind], T](val violationsKindPriority : Seq[Kind],
                                     val graph : AccessGraph[Kind, T],
                                     val solverBuilder : SolverBuilder[Kind, T])
  extends ConstraintSolvingSearchEngineDecisionMaker[Kind, T]
  with FunneledSeachEngine[ResultT[Kind, T]]
  with InitialStateCreator[Kind, T]
