package puck.graph.constraints.search

import puck.graph._
import puck.graph.constraints.DecisionMaker.ChooseNodeKArg
import puck.search.SearchState
import puck.util.Logged

import scala.util.Random


object ConstraintSolvingNodesChoice {

  def apply(k : ChooseNodeKArg => Unit,
            cs : Seq[Option[(DependencyGraph, NodeId)]]) =
    build(k, cs.toSet)

  def includeNoneChoice(k : ChooseNodeKArg => Unit,
                        cs : Seq[Option[(DependencyGraph, NodeId)]])= {
    build(k, cs.toSet + None)
  }

  def build(k : ChooseNodeKArg => Unit, choices : Set[Option[(DependencyGraph, NodeId)]]) =
    new ConstraintSolvingNodesChoice(k, Random.shuffle(choices), Set[Option[(DependencyGraph, NodeId)]]())
}

class ConstraintSolvingNodesChoice private
( val k : ChooseNodeKArg => Unit,
  var remainingChoices : Set[Option[(DependencyGraph, NodeId)]],
  var triedChoices : Set[Option[(DependencyGraph, NodeId)]])
 extends ConstraintSolvingChoice[(DependencyGraph, NodeId), ConstraintSolvingNodesChoice] {
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
extends ConstraintSolvingState[(DependencyGraph, NodeId), ConstraintSolvingNodesChoice]{
  def decorate(c : Option[NodeId]) = (loggedResult.value, c)
}



