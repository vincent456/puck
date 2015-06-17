package puck.scalaGraph.nodeKind

import puck.graph._
import puck.graph.constraints.AbstractionPolicy

trait ScalaNodeKind extends NodeKind
object ScalaNodeKind extends NodeKindKnowledge {
  override def rootKind: NodeKind = ScalaRoot

  override def kindOfKindType(kindType: KindType): Seq[NodeKind] =
    kindType match {
      case NameSpace => Seq(Package)
      case TypeConstructor => Seq(PrimaryConstructor, SecondaryConstructor)
      case TypeDecl => Seq(Trait, Class, Type, Object)
      case TypeMember => Seq(Def, Var, Val)
      case TypeDeclAndTypeMember => Seq(Trait, Class, Object, Type, Def, Var, Val)
      case Unknown => sys.error("Unknown kind type")
    }

  override def nodeKinds: Seq[NodeKind] = Seq(Package, PackageObject, Trait, Type, Object, Var, Val, Def)
}

case object ScalaRoot extends ScalaNodeKind with AGRoot {
  override def canContain(k: NodeKind) = k match {
    case Package => true
    case _ => false
  }
}
case object Package extends ScalaNodeKind {
  def canContain(k : NodeKind) : Boolean = {
    k match {
      case Package
        | Class
        | Trait
        | Object
        | PackageObject => true
      case _ => false
    }
  }

  override def abstractionPolicies = Seq()
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object PackageObject extends ScalaNodeKind {
  def canContain(k : NodeKind) : Boolean = {
    k match {
      case Package
           | PackageObject => false
      case _ => true
    }
  }
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}


trait ScalaTypeDeclKind extends ScalaNodeKind {
  def canContain(k : NodeKind) : Boolean = {
    k match {
      case Package
      | PackageObject => false
      case _ => true
    }
  }
}

case object Class extends ScalaTypeDeclKind {
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object Trait extends ScalaTypeDeclKind {
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object Type extends ScalaNodeKind {

  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()

  override def canContain(k: NodeKind): Boolean = false
}

case object PrimaryConstructor extends ScalaNodeKind {
  def canContain(k : NodeKind) : Boolean = false
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object SecondaryConstructor extends ScalaNodeKind {
  def canContain(k : NodeKind) : Boolean = false
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}


case object Object extends ScalaTypeDeclKind {
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object Val extends ScalaNodeKind {
  def canContain(k : NodeKind) : Boolean = false
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object Var extends ScalaNodeKind {
  def canContain(k : NodeKind) : Boolean = false
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object Def extends ScalaNodeKind {
  def canContain(k : NodeKind) : Boolean = false
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}

