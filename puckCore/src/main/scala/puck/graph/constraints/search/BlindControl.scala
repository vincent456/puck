package puck.graph.constraints.search

import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.TransformationRules
import puck.search.SearchControl


trait Blind extends ActionGenerator {
  def nextStates(violationTarget : ConcreteNode)(g: DependencyGraph) : Seq[LoggedTry[DependencyGraph]] =
    redirectTowardAbstractions(violationTarget)(g) ++
      moveAction(violationTarget)(g) ++
      moveContainerAction(violationTarget)(g) ++
      abstractAction(violationTarget)(g) ++
      hostIntroAction(violationTarget)(g) ++
      abstractContainerAction(violationTarget)(g)
}

class TargetedBlindControl
( val rules: TransformationRules,
  val initialGraph: DependencyGraph,
  val constraints: ConstraintsMaps,
  val virtualNodePolicicy : VirtualNodePolicy,
  val violationTarget : ConcreteNode
) extends SearchControl[DecoratedGraph[Unit]] //DecoratedGraph[Unit] let us share the DecoratedGraphEvalutaor
  with Blind
  with CheckViolation
  with TerminalStateWhenTargetedViolationRemoved[Unit] {
  def initialState: DecoratedGraph[Unit] = (initialGraph, ())

  def nextStates(t: DecoratedGraph[Unit]): Seq[LoggedTry[DecoratedGraph[Unit]]] =
    if(!isViolationTarget(t.graph, violationTarget.id)) Seq()
    else decorate(nextStates(violationTarget)(t.graph), ())


}

import scala.collection.mutable

class BlindControl
(val rules: TransformationRules,
 val initialGraph: DependencyGraph,
 val constraints: ConstraintsMaps,
 val virtualNodePolicicy : VirtualNodePolicy,
 val violationsKindPriority : Seq[NodeKind]
) extends SearchControl[DecoratedGraph[Option[ConcreteNode]]]
  with Blind
  with TargetFinder
  with TerminalStateWhenNoViolations[Option[ConcreteNode]] {

  def initialState: DecoratedGraph[Option[ConcreteNode]] = (initialGraph, None)

  def nextStates(g: DependencyGraph)(violationTarget : ConcreteNode) : Seq[LoggedTry[DecoratedGraph[Option[ConcreteNode]]]] =
    if(constraints.noForbiddenDependencies(g)) Seq()
    else if(!isViolationTarget(g, violationTarget.id)) Seq(LoggedSuccess((g, None)))
    else decorate(nextStates(violationTarget)(g.mileStone), Some(violationTarget))

  private [this] val nextStates_ = mutable.Map[DecoratedGraph[Option[ConcreteNode]],
    Seq[LoggedTry[DecoratedGraph[Option[ConcreteNode]]]]]()

  def nextStates(state : DecoratedGraph[Option[ConcreteNode]]) : Seq[LoggedTry[DecoratedGraph[Option[ConcreteNode]]]] = {

    nextStates_ get state match {
      case None =>
        val ns = state match {
          case (g, Some(violationTarget)) => nextStates(g)(violationTarget)
          case (g, None) => findTargets(g) flatMap nextStates(g)
        }

        nextStates_ += (state -> ns)

        ns

      case Some(ns) =>
        println("nextStates cache hit !")
        ns

    }
  }


}



