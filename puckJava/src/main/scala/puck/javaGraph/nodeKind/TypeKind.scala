package puck.javaGraph.nodeKind


import puck.graph._
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}




sealed abstract class TypeKind extends JavaNodeKind

case object Primitive extends TypeKind {
  override def kindType: KindType = TypeDecl
  def canContain(k: NodeKind) = false
  def abstractionNodeKinds(p : AbstractionPolicy) =
    throw new DGError("do not know how to abstract primitive kind")
}

case object Interface extends TypeKind {

  override def kindType: KindType = TypeDecl

  def canContain(k : NodeKind) : Boolean = {
    k match {
      case AbstractMethod
         | StaticMethod => true
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
    case DelegationAbstraction => Seq()//Class)//also interface ?
  }
}

case object InnerInterface extends TypeKind {

  override def kindType: KindType = InstanceTypeDecl

  def canContain(k : NodeKind) : Boolean = {
    k match {
      case AbstractMethod
           | StaticMethod => true
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
    case SupertypeAbstraction => Seq(Interface, InnerInterface)
    case DelegationAbstraction => Seq()//Class)//also interface ?
  }
}

case object Class extends TypeKind {

  override def kindType: KindType = TypeDecl

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
         | StaticMethod
         | AbstractMethod=> true
      case _ => false
    }
  }

  def abstractionNodeKinds(p : AbstractionPolicy) : Seq[NodeKind] = p match {
    case SupertypeAbstraction => Seq[NodeKind](Interface, Class)
    case DelegationAbstraction => Seq[NodeKind]()//Class)//also interface ?
  }

}

case object InnerClass extends TypeKind {

  override def kindType: KindType = InstanceTypeDecl

  override def canBe(k : NodeKind) : Boolean = {
    k match {
      case InnerClass | Class
           | InnerInterface | Interface => true
      case _ => false
    }
  }

  def canContain(k : NodeKind) : Boolean = {
    k match {
      case Constructor
           | Field
           | Method
           | StaticMethod
           | AbstractMethod => true
      case _ => false
    }
  }

  def abstractionNodeKinds(p : AbstractionPolicy) : Seq[NodeKind] = p match {
    case SupertypeAbstraction =>
      Seq[NodeKind](InnerClass, InnerInterface, Interface, Class)
    case DelegationAbstraction => Seq[NodeKind]()//Class)//also interface ?
  }

}