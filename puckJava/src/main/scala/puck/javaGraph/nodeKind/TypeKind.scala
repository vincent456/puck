package puck.javaGraph.nodeKind


import puck.graph._
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}




sealed abstract class TypeKind extends JavaNodeKind {
  def kindType: KindType = TypeDecl
}

case object Primitive extends TypeKind {
  def canContain(k: NodeKind) = false
  def abstractionNodeKinds(p : AbstractionPolicy) =
    throw new DGError("do not know how to abstract primitive kind")
}

object TypeKind {



  trait InterfaceLike extends TypeKind with Serializable{

    def canContain(k : NodeKind) : Boolean = {
      k match {
        case AbstractMethod
             | StaticMethod => true
        case _ => false
      }
    }
    override def canBe(k : NodeKind) : Boolean = {
      k match {
        case _ : InterfaceLike => true
        case _ => false
      }
    }

    def abstractionNodeKinds(p : AbstractionPolicy) = p match {
      case SupertypeAbstraction => Seq(Interface, GenericInterface)
      case DelegationAbstraction => Seq()//Class)//also interface ?
    }
  }

  trait ClassLike extends TypeKind with Serializable {

    override def canBe(k : NodeKind) : Boolean = {
      k match {
        case _ : ClassLike | _: InterfaceLike => true
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
      case SupertypeAbstraction => Seq[NodeKind](Interface, Class, GenericInterface, GenericClass)
      case DelegationAbstraction => Seq[NodeKind]()//Class)//also interface ?
    }

  }

  trait GenType extends TypeKind with Serializable
}
import TypeKind._
case object Interface extends InterfaceLike {
  override val toString : String = "Interface"
  override def equals(a : Any) : Boolean =
    a.isInstanceOf[InterfaceLike] && !a.isInstanceOf[GenType]

}

case object GenericInterface extends InterfaceLike with GenType {
  override val toString : String = "GenericInterface"
  override def equals(a : Any) : Boolean =
    a.isInstanceOf[InterfaceLike] && a.isInstanceOf[GenType]

}

case object Class extends ClassLike {
  override val toString : String = "Class"
  override def equals(a : Any) : Boolean =
    a.isInstanceOf[ClassLike] && !a.isInstanceOf[GenType]
}


case object GenericClass extends ClassLike with GenType {
  override def equals(a : Any) : Boolean =
    a.isInstanceOf[ClassLike] && a.isInstanceOf[GenType]

  override val toString : String = "GenericClass"
}
//case class GenInterface(numParams : Int /*params : List[Variance]*/)
//  extends TypeKind.Interface with TypeKind.GenType {
//}
//case class GenClass(params : List[Variance])
//  extends TypeKind.Class with TypeKind.GenType {
//}
case object TypeVariable extends TypeKind {
 // def kindType : KindType = InstanceTypeDecl
  def canContain(k : NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object WildCardType extends TypeKind {
  //def kindType : KindType = InstanceTypeDecl
  def canContain(k : NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}