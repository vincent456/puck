package puck.graph.constraints.search

import puck.graph.backTrack.SearchStateBreakPoint
import puck.graph.RedirectionError
import puck.graph.constraints.Solver
import puck.graph.constraints.search.ConstraintSolvingNodesChoice.CSNC
import puck.search.{StateCreator, SearchEngine, SearchState}

import puck.graph.backTrack.Recording
import puck.graph.{AGNode, NodeKind}

import scala.collection.mutable

/**
 * Created by lorilan on 01/07/14.
 */



class ConstraintSolvingNodesChoice[Kind <: NodeKind[Kind]](val recording : Recording[Kind],
                                                           val k : Option[AGNode[Kind]] => Unit,
                                                           val remainingChoices : mutable.Set[AGNode[Kind]],
                                                           val triedChoices : mutable.Set[AGNode[Kind]])
  extends ConstraintSolvingChoice[Kind, AGNode[Kind], CSNC[Kind]] {
  def createState(id : Int,
                  engine : SearchEngine[CSNC[Kind]],
                  prevState : Option[SearchState[_, CSNC[Kind]]],
                  choices : CSNC[Kind]) = {
    new ConstraintSolvingNodeChoiceSearchState(id, engine, choices, prevState)
  }
}

object ConstraintSolvingNodesChoice{
  type CSNC[Kind <: NodeKind[Kind]] = ConstraintSolvingNodesChoice[Kind]
}
/**
 * Created by lorilan on 25/09/14.
 */

class ConstraintSolvingNodeChoiceSearchState[Kind <: NodeKind[Kind]](val id : Int,
                                                                     val engine : SearchEngine[CSNC[Kind]],
                                                                     val internal: CSNC[Kind],
                                                                     val prevState : Option[SearchState[_, CSNC[Kind]]])
extends ConstraintSolvingState[Kind, AGNode[Kind], CSNC[Kind]]

class CSInitialSearchState[Kind <: NodeKind[Kind]](e : SearchEngine[CSNC[Kind]],
                                                   solver : Solver[Kind])
  extends ConstraintSolvingNodeChoiceSearchState[Kind](0, e,
    new ConstraintSolvingNodesChoice(solver.graph.transformations.recording, null,
      mutable.Set(), mutable.Set()), None){

  var executedOnce = false
  override def triedAll = executedOnce
  override def executeNextChoice(){
    //solver.solve(() => printTrace(e.currentState))
    solver.solve()
    executedOnce = true
  }
}