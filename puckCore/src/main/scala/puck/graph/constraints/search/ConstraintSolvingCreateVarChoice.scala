package puck.graph.constraints.search

import puck.graph.ResultT
import puck.graph.transformations.rules.CreateVarStrategy
import puck.search.SearchState
import puck.util.Logged


class ConstraintSolvingCreateVarChoice
(val k : Logged[CreateVarStrategy] => Unit,
 var remainingChoices : Set[CreateVarStrategy],
 var triedChoices : Set[CreateVarStrategy])
extends ConstraintSolvingChoice [CreateVarStrategy, ConstraintSolvingCreateVarChoice]{

  override def createState(givenId: Int,
                           previousState: Option[SearchState[ResultT]],
                           currentResult: Logged[ResultT],
                           choices: ConstraintSolvingCreateVarChoice): SearchState[ResultT] =
    new ConstraintSolvingState[CreateVarStrategy, ConstraintSolvingCreateVarChoice]{
      val id  = givenId
      val loggedResult = currentResult
      val internal = choices
      val prevState = previousState
    }
}


