package puck.graph.constraints.search

import puck.graph.constraints.Solver
import puck.graph.constraints.search.ConstraintSolvingNodesChoice.CSNC
import puck.search.{SearchEngine, SearchState}

import puck.graph.backTrack.Recording
import puck.graph.{AGNode, NodeKind}

import scala.collection.mutable

/**
 * Created by lorilan on 01/07/14.
 */



class ConstraintSolvingNodesChoice[Kind <: NodeKind[Kind]](val k : Option[AGNode[Kind]] => Unit,
                                                           val remainingChoices : mutable.Set[AGNode[Kind]],
                                                           val triedChoices : mutable.Set[AGNode[Kind]])
  extends ConstraintSolvingChoice[Kind, AGNode[Kind], CSNC[Kind]] {
  def createState(id : Int,
                  engine : SearchEngine[Recording[Kind]],
                  prevState : Option[SearchState[Recording[Kind], _]],
                  currentResult: Recording[Kind],
                  choices : CSNC[Kind]) = {
    new ConstraintSolvingNodeChoiceSearchState(id, currentResult, engine, choices, prevState)
  }


}

object ConstraintSolvingNodesChoice{
  type CSNC[Kind <: NodeKind[Kind]] = ConstraintSolvingNodesChoice[Kind]
}
/**
 * Created by lorilan on 25/09/14.
 */

class ConstraintSolvingNodeChoiceSearchState[Kind <: NodeKind[Kind]](val id : Int,
                                                                     val result : Recording[Kind],
                                                                     val engine : SearchEngine[Recording[Kind]],
                                                                     val internal: CSNC[Kind],
                                                                     val prevState : Option[SearchState[Recording[Kind], _]])
extends ConstraintSolvingState[Kind, AGNode[Kind], CSNC[Kind]]{
  //override protected def needToTryNone = true
}



