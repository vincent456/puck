package puck.graph.constraints.search

import puck.graph.transformations.Transformation
import puck.graph.{ResultT, DependencyGraph, NodeKind}
import puck.search.FunneledSeachEngine


class FunneledCSSE
(initialRecord :  Seq[Transformation],
 val violationsKindPriority : Seq[NodeKind],
 val graph : DependencyGraph,
 val solverBuilder : SolverBuilder,
 val automaticConstraintLoosening : Boolean)
  extends ConstraintSolvingSearchEngineDecisionMaker
  with FunneledSeachEngine[ResultT]
  with InitialStateCreator {
  val evaluator = new ConstraintSolvingStateEvaluator(initialRecord)
}
