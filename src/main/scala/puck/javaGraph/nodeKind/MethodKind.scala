package puck.javaGraph.nodeKind

import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.graph.NodeKind

/**
 * Created by lorilan on 31/07/14.
 */
trait MethodKind extends JavaNodeKind {
  def canContain(k : NodeKind) = false
  override def isTypeMember : Boolean = true
}

case object Method extends MethodKind {

  def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction => Seq(AbstractMethod, Method)
    case DelegationAbstraction => Seq(Method)//also abstractMethod ?
  }
}

case object ConstructorMethod extends MethodKind {

  def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction => Seq(AbstractMethod, Method)
    case DelegationAbstraction => Seq(Method)//also abstractMethod ?
  }
}


case object AbstractMethod extends MethodKind {

 def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction => Seq(AbstractMethod)
    case DelegationAbstraction => Seq(Method)//also abstractMethod ?
  }
}