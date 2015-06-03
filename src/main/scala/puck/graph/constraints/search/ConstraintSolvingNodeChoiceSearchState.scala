package puck.graph.constraints.search

import puck.graph._
import puck.search.SearchState

import scala.util.Random


object ConstraintSolvingNodesChoice {
  def apply(k : Option[NodeId] => Unit,
            ids : Seq[NodeId]) =
    build(k, Set[Option[NodeId]]() ++ (ids map (Some(_))))

  def includeNoneChoice(k : Option[NodeId] => Unit,
                        ids : Seq[NodeId])= {
    val size = ids.size
    build(k, Set[Option[NodeId]]() ++ (ids map (Some(_))) /*+ None*/)
  }

  def build(k : Option[NodeId] => Unit, choices : Set[Option[NodeId]]) =
    new ConstraintSolvingNodesChoice(k, Random.shuffle(choices), Set[Option[NodeId]]())
}

class ConstraintSolvingNodesChoice private
( val k : Option[NodeId] => Unit,
  var remainingChoices : Set[Option[NodeId]],
  var triedChoices : Set[Option[NodeId]])
 extends ConstraintSolvingChoice[NodeId, ConstraintSolvingNodesChoice] {
  def createState(id : Int,
                  prevState : Option[SearchState[ResultT]],
                  currentResult: Logged[ResultT],
                  choices : ConstraintSolvingNodesChoice) = {
    new ConstraintSolvingNodeChoiceSearchState(id, currentResult, choices, prevState)
  }
}




class ConstraintSolvingNodeChoiceSearchState
(val id : Int,
 val loggedResult : Logged[ResultT],
 val internal: ConstraintSolvingNodesChoice,
 val prevState : Option[SearchState[ResultT]])
extends ConstraintSolvingState[NodeId, ConstraintSolvingNodesChoice]



