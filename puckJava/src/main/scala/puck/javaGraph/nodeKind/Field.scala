package puck.javaGraph
package nodeKind

import puck.graph.{InstanceValueDecl, StaticValueDecl, KindType, NodeKind}
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}


case object StaticField extends JavaNodeKind{
  override def canBeReadOrWrote = true

  def canContain(k : NodeKind) = false

  def abstractionNodeKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction => Seq()
    case DelegationAbstraction => Seq(StaticMethod)
  }
  override def abstractionPolicies = Seq(DelegationAbstraction)

  override def kindType: KindType = StaticValueDecl
}

case object Field extends JavaNodeKind {

  override def kindType: KindType = InstanceValueDecl
  override def canBeReadOrWrote = true

  def canContain(k : NodeKind) = false

  // fieldread abstraction type = () -> t
  // fielwrite abstraction type = t -> ()
  def abstractionNodeKinds(p : AbstractionPolicy) = p match {
      case SupertypeAbstraction => Seq()
      case DelegationAbstraction => Seq(Method)
    }


  override def abstractionPolicies = Seq(DelegationAbstraction)
}
