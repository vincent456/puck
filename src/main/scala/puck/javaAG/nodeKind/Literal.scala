package puck.javaAG.nodeKind

import puck.graph.{NamedType, HasType}
import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction}
import puck.javaAG.JavaNamedType

/**
 * Created by lorilan on 31/07/14.
 */
case class Literal private[javaAG]() extends JavaNodeKind with HasType[JavaNodeKind, NamedType[JavaNodeKind]]{
  override val toString = "Literal"

  def create() = JavaNodeKind.literal(typ)

  def canContain(k : JavaNodeKind) = false
  //TODO in case of method abstraction cf field comment
  override def abstractionPolicies = List(DelegationAbstraction())
  def abstractKinds(p : AbstractionPolicy) = List(JavaNodeKind.field(typ), Method())
}
