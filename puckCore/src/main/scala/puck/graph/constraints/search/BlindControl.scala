package puck.graph.constraints.search

import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.TransformationRules
import puck.search.SearchControl



class BlindControl
(val rules: TransformationRules,
 val initialGraph: DependencyGraph,
 val constraints: ConstraintsMaps,
 val violationsKindPriority : Seq[NodeKind]
) extends SearchControl[(DependencyGraph, Option[ConcreteNode])]
  with ActionGenerator
  with TargetFinder {

  def initialState: (DependencyGraph, Option[ConcreteNode]) = (initialGraph, None)

  def nextStates(g: DependencyGraph)(violationTarget : ConcreteNode) : Seq[LoggedTry[(DependencyGraph, Option[ConcreteNode])]] =
    if(!isViolationTarget(g, violationTarget.id)) Seq(LoggedSuccess((g, None)))
    else setState((redirectTowardAbstractions(violationTarget)(g) ++
      moveAction(violationTarget)(g) ++
      moveContainerAction(violationTarget)(g) ++
      absIntro(violationTarget)(g) ++
      hostIntroAction(violationTarget)(g) ++
      hostAbsIntro(violationTarget)(g), Some(violationTarget)))


  def nextStates(state : (DependencyGraph, Option[ConcreteNode])) : Seq[LoggedTry[(DependencyGraph, Option[ConcreteNode])]] =
  state match {
    case (g, Some(violationTarget)) => nextStates(g)(violationTarget)
    case (g, None) => findTargets(g) flatMap nextStates(g)

 }
}



