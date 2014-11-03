package puck.graph.constraints.search

import puck.graph.{ResultT, NodeKind}
import puck.graph.constraints.AbstractionPolicy
import puck.search.{SearchEngine, SearchState}

import scala.collection.mutable

/**
 * Created by lorilan on 25/09/14.
 */
class ConstraintSolvingAbstractionChoice[Kind <: NodeKind[Kind], T]
(val k : Option[(Kind, AbstractionPolicy)] => Unit,
 val remainingChoices : mutable.Set[(Kind, AbstractionPolicy)],
 val triedChoices : mutable.Set[(Kind, AbstractionPolicy)])
extends ConstraintSolvingChoice[Kind, (Kind, AbstractionPolicy), T, ConstraintSolvingAbstractionChoice[Kind, T]] {
  def createState(id: Int,
                  engine: SearchEngine[ResultT[Kind, T]],
                  prevState: Option[SearchState[ResultT[Kind, T], _]],
                  currentResult : ResultT[Kind, T],
                  choices: ConstraintSolvingAbstractionChoice[Kind, T]) =
    new ConstraintSolvingAbstractionChoiceSearchState[Kind, T](id, currentResult, engine, choices, prevState)

}


class ConstraintSolvingAbstractionChoiceSearchState[Kind <: NodeKind[Kind], T]
(val id : Int,
 val result : ResultT[Kind, T],
 val engine : SearchEngine[ResultT[Kind, T]],
 val internal: ConstraintSolvingAbstractionChoice[Kind, T],
 val prevState : Option[SearchState[ResultT[Kind, T],_]])
extends ConstraintSolvingState[Kind, (Kind, AbstractionPolicy), T, ConstraintSolvingAbstractionChoice[Kind, T]]
