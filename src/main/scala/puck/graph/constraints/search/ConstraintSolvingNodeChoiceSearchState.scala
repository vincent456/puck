package puck.graph.constraints.search

import puck.graph._
import puck.search.{SearchEngine, SearchState}

import scala.collection.mutable

/**
 * Created by lorilan on 01/07/14.
 */



class ConstraintSolvingNodesChoice[Kind <: NodeKind[Kind], T]
(val k : Option[NodeId[Kind]] => Unit,
 val remainingChoices : mutable.Set[NodeId[Kind]],
 val triedChoices : mutable.Set[NodeId[Kind]])
 extends ConstraintSolvingChoice[Kind, NodeId[Kind], T, ConstraintSolvingNodesChoice[Kind, T]] {
  def createState(id : Int,
                  engine : SearchEngine[ResultT[Kind, T]],
                  prevState : Option[SearchState[ResultT[Kind, T], _]],
                  currentResult: ResultT[Kind, T],
                  choices : ConstraintSolvingNodesChoice[Kind, T]) = {
    new ConstraintSolvingNodeChoiceSearchState(id, currentResult, engine, choices, prevState)
  }


}

object ConstraintSolvingNodesChoice{
  type CSNC[Kind <: NodeKind[Kind], T] = ConstraintSolvingNodesChoice[Kind, T]
}

/**
 * Created by lorilan on 25/09/14.
 */

class ConstraintSolvingNodeChoiceSearchState[Kind <: NodeKind[Kind], T]
(val id : Int,
 val result : ResultT[Kind, T],
 val engine : SearchEngine[ResultT[Kind, T]],
 val internal: ConstraintSolvingNodesChoice[Kind, T],
 val prevState : Option[SearchState[ResultT[Kind, T], _]])
extends ConstraintSolvingState[Kind, NodeId[Kind], T, ConstraintSolvingNodesChoice[Kind, T]]{
  //override protected def needToTryNone = true
}



