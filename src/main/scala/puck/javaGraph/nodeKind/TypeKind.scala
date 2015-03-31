package puck.javaGraph.nodeKind

import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.graph.NodeKind

/**
 * Created by lorilan on 31/07/14.
 */
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

  def abstractKinds(p : AbstractionPolicy) = p match {
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
         | ConstructorMethod => true
      case _ => false
    }
  }

  def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction => Seq(Interface, Class)
    case DelegationAbstraction => Seq(Class)//also interface ?
  }



}