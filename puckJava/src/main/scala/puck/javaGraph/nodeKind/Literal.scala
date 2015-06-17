package puck.javaGraph.nodeKind

import puck.graph.NodeKind
import puck.graph.constraints.{DelegationAbstraction, AbstractionPolicy}


/**
 * Created by lorilan on 31/07/14.
 */
case object Literal extends JavaNodeKind {
  override val toString = "Literal"

  def canContain(k : NodeKind) = false
  //TODO in case of method abstraction cf field comment
  override def abstractionPolicies = Seq(DelegationAbstraction)
  def abstractionNodeKinds(p : AbstractionPolicy) = ???
    ///List(Field(puck.graph.dummyId, typ), Method())
}
