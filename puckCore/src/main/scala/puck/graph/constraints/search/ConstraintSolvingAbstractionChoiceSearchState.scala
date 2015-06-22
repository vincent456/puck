package puck.graph.constraints.search

import puck.graph.{ResultT, NodeKind}
import puck.graph.constraints.AbstractionPolicy
import puck.search.SearchState
import puck.util.Logged

class ConstraintSolvingAbstractionChoice
( val k : Logged[Option[(NodeKind, AbstractionPolicy)]] => Unit,
  var remainingChoices : Set[Option[(NodeKind, AbstractionPolicy)]],
  var triedChoices : Set[Option[(NodeKind, AbstractionPolicy)]])
extends ConstraintSolvingChoice[Option[(NodeKind, AbstractionPolicy)], ConstraintSolvingAbstractionChoice] {
  def createState(id: Int,
                  prevState: Option[SearchState[ResultT]],
                  currentResult : Logged[ResultT],
                  choices: ConstraintSolvingAbstractionChoice) =
    new ConstraintSolvingAbstractionChoiceSearchState(id, currentResult, choices, prevState)

}


class ConstraintSolvingAbstractionChoiceSearchState
(val id : Int,
 val loggedResult : Logged[ResultT],
 val internal: ConstraintSolvingAbstractionChoice,
 val prevState : Option[SearchState[ResultT]])
extends ConstraintSolvingState[Option[(NodeKind, AbstractionPolicy)], ConstraintSolvingAbstractionChoice]
