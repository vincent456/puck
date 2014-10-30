package puck.javaAG.immutable.nodeKind

import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction}
import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable.{NamedType, HasType}

/**
 * Created by lorilan on 31/07/14.
 */
case class Literal private[javaAG](node : NodeId[JavaNodeKind], typ : NamedType[JavaNodeKind])
  extends JavaNodeKind with HasType[JavaNodeKind, NamedType[JavaNodeKind]]{
  override val toString = "Literal"

  def create(node : NodeId[JavaNodeKind]) = Literal(node, typ)

  def canContain(k : JavaNodeKind) = false
  //TODO in case of method abstraction cf field comment
  override def abstractionPolicies = List(DelegationAbstraction())
  def abstractKinds(p : AbstractionPolicy) = ???
    ///List(Field(puck.graph.dummyId, typ), Method())
}
