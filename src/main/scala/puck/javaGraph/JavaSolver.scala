package puck.javaGraph

import puck.graph.{DGNode, NodeKind, DependencyGraph}
import puck.graph.constraints.{AbstractionPolicy, DecisionMaker, Solver, SupertypeAbstraction}
import puck.javaGraph.nodeKind._
import puck.javaGraph.transformations.JavaTransformationRules


/**
 * Created by lorilan on 28/05/14.
 */

object JavaSolver{
  def apply(graph : DependencyGraph,
            decisionMaker : DecisionMaker,
            automaticConstraintLoosening : Boolean) =
    new JavaSolver(graph, decisionMaker, automaticConstraintLoosening)

  val violationPrioritySeq =
              Seq[JavaNodeKind]( Field, Constructor, Class, Interface)
}

class JavaSolver(val graph : DependencyGraph,
                 val decisionMaker : DecisionMaker,
                 val automaticConstraintLoosening : Boolean) extends Solver{

  val logger = graph.logger

  val rules = JavaTransformationRules

  override def absIntroPredicate( graph : GraphT,
                                  impl : DGNode,
                                  absPolicy : AbstractionPolicy,
                                  absKind : NodeKind) : PredicateT = {
    (impl.kind, absPolicy) match {
      case (Method, SupertypeAbstraction)
           | (AbstractMethod, SupertypeAbstraction) =>
        (graph, potentialHost) => !graph.interloperOf(graph.container(impl.id).get, potentialHost.id)
      case _ => super.absIntroPredicate(graph, impl, absPolicy, absKind)
    }
  }
}
