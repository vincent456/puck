package puck.graph.constraints.search

import puck.graph.{ResultT, NodeKind}
import puck.graph.constraints.AbstractionPolicy
import puck.search.{SearchEngine, SearchState}

/**
 * Created by lorilan on 25/09/14.
 */

class ConstraintSolvingAbstractionChoice
( val k : Option[(NodeKind, AbstractionPolicy)] => Unit,
  var remainingChoices : Set[Option[(NodeKind, AbstractionPolicy)]],
  var triedChoices : Set[Option[(NodeKind, AbstractionPolicy)]])
extends ConstraintSolvingChoice[(NodeKind, AbstractionPolicy), ConstraintSolvingAbstractionChoice] {
  def createState(id: Int,
                  engine: SearchEngine[ResultT],
                  prevState: Option[SearchState[ResultT]],
                  currentResult : ResultT,
                  choices: ConstraintSolvingAbstractionChoice) =
    new ConstraintSolvingAbstractionChoiceSearchState(id, currentResult, engine, choices, prevState)

}


class ConstraintSolvingAbstractionChoiceSearchState
(val id : Int,
 val result : ResultT,
 val engine : SearchEngine[ResultT],
 val internal: ConstraintSolvingAbstractionChoice,
 val prevState : Option[SearchState[ResultT]])
extends ConstraintSolvingState[(NodeKind, AbstractionPolicy), ConstraintSolvingAbstractionChoice]
