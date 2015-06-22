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

  override def createState(id: Int,
                           prevState: Option[SearchState[ResultT]],
                           currentResult: Logged[ResultT],
                           choices: ConstraintSolvingCreateVarChoice): SearchState[ResultT] =
  new ConstraintSolvingCreateVarSearchState(id, currentResult, choices, prevState)
}

class ConstraintSolvingCreateVarSearchState
(val id : Int,
 val loggedResult : Logged[ResultT],
 val internal: ConstraintSolvingCreateVarChoice,
 val prevState : Option[SearchState[ResultT]])
  extends ConstraintSolvingState[CreateVarStrategy, ConstraintSolvingCreateVarChoice]
