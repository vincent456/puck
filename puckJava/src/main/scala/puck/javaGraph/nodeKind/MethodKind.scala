package puck.javaGraph.nodeKind

import puck.graph.NodeKind
import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction, SupertypeAbstraction}

trait MethodKind extends JavaNodeKind {
  def canContain(k : NodeKind) = false
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