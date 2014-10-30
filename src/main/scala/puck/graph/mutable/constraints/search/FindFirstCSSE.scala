package puck.graph.mutable.constraints.search

import puck.graph.mutable.backTrack.Recording
import puck.graph.mutable.{AccessGraph, NodeKind}
import puck.search.FindFirstSearchEngine

/**
 * Created by lorilan on 25/09/14.
 */
//CSSE : Constraint Solving Search Engine
class FindFirstCSSE[Kind <: NodeKind[Kind]](val violationsKindPriority : List[Kind],
                                            val graph : AccessGraph[Kind],
                                             sb : SolverBuilder[Kind])
  extends ConstraintSolvingSearchEngineDecisionMaker[Kind](sb)
  with FindFirstSearchEngine[Recording[Kind]]
