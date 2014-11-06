package puck.graph.constraints.search

import puck.graph._
import puck.search.{SearchEngine, SearchState}

import scala.collection.mutable

/**
 * Created by lorilan on 01/07/14.
 */



class ConstraintSolvingNodesChoice
(val k : Option[NodeId] => Unit,
 val remainingChoices : mutable.Set[NodeId],
 val triedChoices : mutable.Set[NodeId])
 extends ConstraintSolvingChoice[NodeId, ConstraintSolvingNodesChoice] {
  def createState(id : Int,
                  engine : SearchEngine[ResultT],
                  prevState : Option[SearchState[ResultT]],
                  currentResult: ResultT,
                  choices : ConstraintSolvingNodesChoice) = {
    new ConstraintSolvingNodeChoiceSearchState(id, currentResult, engine, choices, prevState)
  }


}

/**
 * Created by lorilan on 25/09/14.
 */

class ConstraintSolvingNodeChoiceSearchState
(val id : Int,
 val result : ResultT,
 val engine : SearchEngine[ResultT],
 val internal: ConstraintSolvingNodesChoice,
 val prevState : Option[SearchState[ResultT]])
extends ConstraintSolvingState[NodeId, ConstraintSolvingNodesChoice]{
  //override protected def needToTryNone = true
}



