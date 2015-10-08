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
  def createState(givenId: Int,
                  previousState: Option[SearchState[ResultT]],
                  currentResult : Logged[ResultT],
                  choices: ConstraintSolvingAbstractionChoice) =
    new ConstraintSolvingState[Option[(NodeKind, AbstractionPolicy)], ConstraintSolvingAbstractionChoice]{
      val id = givenId
      val loggedResult = currentResult
      val internal = choices
      val prevState = previousState
    }


}
