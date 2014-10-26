package puck.graph.constraints.search

import puck.graph.backTrack.Recording
import puck.graph.{AccessGraph, NodeKind}
import puck.search.FunneledSeachEngine

/**
 * Created by lorilan on 26/10/14.
 */
class FunneledCSSE[Kind <: NodeKind[Kind]](val violationsKindPriority : List[Kind],
                                     val graph : AccessGraph[Kind],
                                     sb : SolverBuilder[Kind])
  extends ConstraintSolvingSearchEngineDecisionMaker[Kind](sb)
  with FunneledSeachEngine[Recording[Kind]]
