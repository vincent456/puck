package puck.javaGraph.nodeKind

import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.graph.NodeKind


abstract class TypeKind extends JavaNodeKind

case object Interface extends TypeKind {

  def canContain(k : NodeKind) : Boolean = {
    k match {
      case AbstractMethod => true
      case _ => false
    }
  }

  override def canBe(k : NodeKind) : Boolean = {
    k match {
      case Interface => true
      case _ => false
    }
  }

  def abstractionNodeKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction => Seq(Interface)
    case DelegationAbstraction => Seq(Class)//also interface ?
  }

}

case object Class extends TypeKind {

  override def canBe(k : NodeKind) : Boolean = {
    k match {
      case Class | Interface => true
      case _ => false
    }
  }

  def canContain(k : NodeKind) : Boolean = {
    k match {
      case Constructor
         | Field
         | Method
         | ConstructorMethod
         | AbstractMethod=> true
      case _ => false
    }
  }

  def abstractionNodeKinds(p : AbstractionPolicy) : Seq[NodeKind] = p match {
    case SupertypeAbstraction => Seq[NodeKind](Interface, Class)
    case DelegationAbstraction => Seq[NodeKind](Class)//also interface ?
  }



}