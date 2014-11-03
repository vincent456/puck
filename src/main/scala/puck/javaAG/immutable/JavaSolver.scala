package puck.javaAG.immutable

import puck.graph.constraints.{AbstractionPolicy, DecisionMaker, Solver, SupertypeAbstraction}
import puck.graph.immutable.AccessGraph
import puck.javaAG.immutable.nodeKind._


/**
 * Created by lorilan on 28/05/14.
 */

object JavaSolver{
  def apply(graph : AccessGraph[JavaNodeKind, DeclHolder],
            decisionMaker : DecisionMaker[JavaNodeKind, DeclHolder]) = new JavaSolver(graph, decisionMaker)

  val violationPrioritySeq =
              Seq[JavaNodeKind]( Field, Constructor, Class, Interface)
}

class JavaSolver(val graph : AccessGraph[JavaNodeKind, DeclHolder],
                 val decisionMaker : DecisionMaker[JavaNodeKind, DeclHolder]) extends Solver[JavaNodeKind, DeclHolder]{

  val logger = graph.logger

  override def absIntroPredicate( graph : GraphT,
                                  implId : NIdT,
                                  absPolicy : AbstractionPolicy,
                                  absKind : JavaNodeKind) : PredicateT = {
    (graph.getNode(implId).kind, absPolicy) match {
      case (Method, SupertypeAbstraction)
           | (AbstractMethod, SupertypeAbstraction) =>
        (graph, potentialHost) => !graph.interloperOf(graph.container(implId), potentialHost)
      case _ => super.absIntroPredicate(graph, implId, absPolicy, absKind)
    }
  }
}
