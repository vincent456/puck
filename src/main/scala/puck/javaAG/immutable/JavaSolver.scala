package puck.javaAG.immutable

import puck.graph.constraints.{AbstractionPolicy, DecisionMaker, Solver, SupertypeAbstraction}
import puck.graph.immutable.{NodeKind, AccessGraph}
import puck.javaAG.immutable.nodeKind._


/**
 * Created by lorilan on 28/05/14.
 */

object JavaSolver{
  def apply(graph : AccessGraph,
            decisionMaker : DecisionMaker) = new JavaSolver(graph, decisionMaker)

  val violationPrioritySeq =
              Seq[JavaNodeKind]( Field, Constructor, Class, Interface)
}

class JavaSolver(val graph : AccessGraph,
                 val decisionMaker : DecisionMaker) extends Solver{

  val logger = graph.logger

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
