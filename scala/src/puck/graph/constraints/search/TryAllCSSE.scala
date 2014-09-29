package puck.graph.constraints.search

import puck.graph.{AGNode, AccessGraph, NodeKind}
import puck.search.{SearchState, TryAllSearchEngine}
import puck.util.Logger

/**
 * Created by lorilan on 25/09/14.
 */
//CSSE : Constraint Solving Search Engine
class TryAllCSSE[Kind <: NodeKind[Kind]](
  val logger : Logger[Int],
  val violationsKindPriority : List[Kind],
  val graph : AccessGraph[Kind],
  sb : SolverBuilder[Kind],
  solverLogger : Logger[Int])
  extends ConstraintSolvingSearchEngineDecisionMaker[Kind](
     sb, solverLogger)
  with TryAllSearchEngine[ConstraintSolvingNodesChoice[Kind]] {


  /*override def newCurrentState(k: Option[AGNode[Kind]] => Unit,
                               choices : ConstraintSolvingChoices[Kind]) {
    //create new state only if there is not one that produces the same graph        1
    if(currentState.internal.recording.nonEmpty){
      initialState.iterator.find(s =>
        !(s eq currentState) &&
          s.internal.recording.produceSameGraph(currentState.internal.recording)
      ) match {
        case Some(s) =>
          logger.writeln("states %s and %s have same graph".format(s.uuid(), currentState.uuid()))
        case None => super.newCurrentState(k, choices)
      }
    }
    else{
      super.newCurrentState(k, choices)
    }
  }*/


}
