/*
package puck.graph.mutable.constraints.search

import puck.graph.{AGNode, AccessGraph, NodeKind}
import puck.search.{GradedSearchEngine, SearchState}
import puck.util.Logger

/**
 * Created by lorilan on 25/09/14.
 */
//CSSE : Constraint Solving Search Engine
class GradedCSSE[Kind <: NodeKind[Kind]](
 val logger : Logger[Int],
 val violationsKindPriority : List[Kind],
 val graph : AccessGraph[Kind],
 sb : SolverBuilder[Kind],
 solverLogger : Logger[Int],
 printTrace : SearchState[ConstraintSolvingNodesChoice[Kind],
   Option[AGNode[Kind]]] => Unit)
  extends ConstraintSolvingSearchEngineDecisionMaker[Kind](
    sb, solverLogger, printTrace )
  with GradedSearchEngine[ConstraintSolvingNodesChoice[Kind], Option[AGNode[Kind]]] {

  def grade(st : SearchState[ConstraintSolvingNodesChoice[Kind], Option[AGNode[Kind]]]) =
    (st.internal.recording.graph.coupling * 1000 ).toInt
}
*/
