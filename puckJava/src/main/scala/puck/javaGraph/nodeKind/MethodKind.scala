package puck.javaGraph.nodeKind

import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.graph.NodeKind

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