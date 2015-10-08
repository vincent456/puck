package puck.graph.constraints.search

import puck.graph.constraints.DecisionMaker.ChooseNodeKArg
import puck.graph.{ResultT, DependencyGraph, NodeId}
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
 extends ConstraintSolvingChoice[Option[(DependencyGraph, NodeId)], ConstraintSolvingNodesChoice] {
  def createState(givenId : Int,
                  previousState : Option[SearchState[ResultT]],
                  currentResult: Logged[ResultT],
                  choices : ConstraintSolvingNodesChoice) =
    new ConstraintSolvingState[Option[(DependencyGraph, NodeId)], ConstraintSolvingNodesChoice]{
      val id  = givenId
      val loggedResult  = currentResult
      val internal = choices
      val prevState  = previousState
    }

}




