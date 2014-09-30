package puck.graph.constraints.search

import puck.graph.NodeKind
import puck.graph.backTrack.Recording
import puck.graph.constraints.AbstractionPolicy
import puck.graph.constraints.search.ConstraintSolving.NodeChoice
import puck.search.{SearchState, SearchEngine}

import scala.collection.mutable

object ConstraintSolvingAbstractionChoice{
  type CSAC[Kind <: NodeKind[Kind]] = ConstraintSolvingAbstractionChoice[Kind]
}

import ConstraintSolvingAbstractionChoice.CSAC

/**
 * Created by lorilan on 25/09/14.
 */
class ConstraintSolvingAbstractionChoice[Kind <: NodeKind[Kind]](val k : Option[(Kind, AbstractionPolicy)] => Unit,
                                                                 val remainingChoices : mutable.Set[(Kind, AbstractionPolicy)],
                                                                 val triedChoices : mutable.Set[(Kind, AbstractionPolicy)])
  extends ConstraintSolvingChoice[Kind, (Kind, AbstractionPolicy), CSAC[Kind]] {
  def createState(id: Int,
                  engine: SearchEngine[Recording[Kind]],
                  prevState: Option[SearchState[Recording[Kind], _]],
                  currentResult : Recording[Kind],
                  choices: CSAC[Kind]) =
    new ConstraintSolvingAbstractionChoiceSearchState[Kind](id, currentResult, engine, choices, prevState)

}


class ConstraintSolvingAbstractionChoiceSearchState[Kind <: NodeKind[Kind]](val id : Int,
                                                                            val result : Recording[Kind],
                                                                            val engine : SearchEngine[Recording[Kind]],
                                                                            val internal: CSAC[Kind],
                                                                            val prevState : Option[SearchState[Recording[Kind],_]])
  extends ConstraintSolvingState[Kind, (Kind, AbstractionPolicy), CSAC[Kind]]
