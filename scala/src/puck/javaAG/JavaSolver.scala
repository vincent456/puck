package puck.javaAG

import puck.graph.constraints._
import puck.graph.AccessGraph._
import puck.graph._
import puck.javaAG.JavaNodeKind._
import puck.graph.constraints.SupertypeAbstraction
import puck.graph.constraints.SupertypeAbstraction
import puck.graph.constraints.DelegationAbstraction
import puck.javaAG.JavaNodeKind.Field
import puck.javaAG.JavaNodeKind.Interface
import puck.javaAG.JavaNodeKind.Method
import puck.javaAG.JavaNodeKind.Class

/**
 * Created by lorilan on 28/05/14.
 */
class JavaSolver(val graph : AccessGraph,
                 val decisionMaker : DecisionMaker) extends Solver{

  override  def singleAbsIntroPredicate(impl : AGNode,
                                        absPolicy : AbstractionPolicy,
                                        absKind : NodeKind) : AGNode => Boolean =
    (impl.kind, absPolicy) match {
    case (Method(), SupertypeAbstraction())
    | (AbstractMethod(), SupertypeAbstraction()) =>
      potentialHost => !(impl.container interloperOf potentialHost)
    case _ => super.singleAbsIntroPredicate(impl, absPolicy, absKind)
  }

}
