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
class ConstraintSolvingAbstractionChoice[Kind <: NodeKind[Kind]](val recording : Recording[Kind],
                                                                 val k : Option[(Kind, AbstractionPolicy)] => Unit,
                                                                 val remainingChoices : mutable.Set[(Kind, AbstractionPolicy)],
                                                                 val triedChoices : mutable.Set[(Kind, AbstractionPolicy)])
  extends ConstraintSolvingChoice[Kind, (Kind, AbstractionPolicy), CSAC[Kind]] {
  def createState(id: Int,
                  engine: SearchEngine[NodeChoice[Kind]],
                  prevState: Option[SearchState[_, NodeChoice[Kind]]],
                  choices: CSAC[Kind]) =
    new ConstraintSolvingAbstractionChoiceSearchState[Kind](id, engine, choices, prevState)

}


class ConstraintSolvingAbstractionChoiceSearchState[Kind <: NodeKind[Kind]](val id : Int,
                                                                            val engine : SearchEngine[NodeChoice[Kind]],
                                                                            val internal: CSAC[Kind],
                                                                            val prevState : Option[SearchState[_, NodeChoice[Kind]]])
  extends ConstraintSolvingState[Kind, (Kind, AbstractionPolicy), CSAC[Kind]]
