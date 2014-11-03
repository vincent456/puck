package puck.graph.constraints.search

import puck.graph.{ResultT, AccessGraph, NodeKind}
import puck.search.TryAllSearchEngine

/**
 * Created by lorilan on 25/09/14.
 */
//CSSE : Constraint Solving Search Engine
class TryAllCSSE[Kind <: NodeKind[Kind], T](
  val violationsKindPriority : Seq[Kind],
  val graph : AccessGraph[Kind, T],
  val solverBuilder : SolverBuilder[Kind, T])
  extends ConstraintSolvingSearchEngineDecisionMaker[Kind, T]
  with TryAllSearchEngine[ResultT[Kind, T]]
  with InitialStateCreator[Kind, T]
