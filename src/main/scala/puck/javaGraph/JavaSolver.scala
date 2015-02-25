package puck.javaGraph

import puck.graph.{NodeKind, DependencyGraph}
import puck.graph.constraints.{AbstractionPolicy, DecisionMaker, Solver, SupertypeAbstraction}
import puck.javaGraph.nodeKind._


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
                                  implId : NIdT,
                                  absPolicy : AbstractionPolicy,
                                  absKind : NodeKind) : PredicateT = {
    (graph.getNode(implId).kind, absPolicy) match {
      case (Method, SupertypeAbstraction)
           | (AbstractMethod, SupertypeAbstraction) =>
        (graph, potentialHost) => !graph.interloperOf(graph.container(implId).get, potentialHost)
      case _ => super.absIntroPredicate(graph, implId, absPolicy, absKind)
    }
  }
}
