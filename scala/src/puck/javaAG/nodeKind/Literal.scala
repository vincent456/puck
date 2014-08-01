package puck.javaAG.nodeKind

import puck.graph.HasType
import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction}
import puck.javaAG.JavaType

/**
 * Created by lorilan on 31/07/14.
 */
case class Literal private[javaAG]() extends JavaNodeKind with HasType[JavaType]{
  override val toString = "Literal"

  def create() = JavaNodeKind.literal(`type`)

  def canContain(k : JavaNodeKind) = false
  //TODO in case of method abstraction cf field comment
  override def abstractionPolicies = List(DelegationAbstraction())
  def abstractKinds(p : AbstractionPolicy) = List(JavaNodeKind.field(`type`), Method())
}
