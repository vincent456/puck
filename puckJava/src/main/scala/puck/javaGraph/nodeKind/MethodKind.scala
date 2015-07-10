package puck.javaGraph.nodeKind

import puck.graph._
import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction, SupertypeAbstraction}


case object StaticMethod extends JavaNodeKind {
  def canContain(k : NodeKind) = false
  override def kindType : KindType = StaticValueDecl
  def abstractionNodeKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction => Seq()
    case DelegationAbstraction => Seq(StaticMethod)
  }
}

trait MethodKind extends JavaNodeKind {
  def canContain(k : NodeKind) = false
  override def kindType : KindType = InstanceValueDecl
}

case object Method extends MethodKind {

  def abstractionNodeKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction => Seq(AbstractMethod, Method)
    case DelegationAbstraction => Seq(Method)//also abstractMethod ?
  }
}

case object ConstructorMethod extends MethodKind {

  def abstractionNodeKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction => Seq(AbstractMethod, Method)
    case DelegationAbstraction => Seq(Method)//also abstractMethod ?
  }
}


case object AbstractMethod extends MethodKind {

 def abstractionNodeKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction => Seq(AbstractMethod)
    case DelegationAbstraction => Seq(Method)//also abstractMethod ?
  }
}