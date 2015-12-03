package puck.javaGraph.nodeKind


import puck.graph.{TypeDecl, KindType, NodeKind}
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}


abstract class TypeKind extends JavaNodeKind {
  override def kindType: KindType = TypeDecl
}

case object Interface extends TypeKind {

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