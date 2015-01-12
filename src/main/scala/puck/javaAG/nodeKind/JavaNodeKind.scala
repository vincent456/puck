package puck.javaAG
package nodeKind


import puck.graph._
import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction}

/**
 * Created by lorilan on 31/07/14.
 */
abstract class JavaNodeKind extends NodeKind{
  /*def packageNode : AGNode[JavaNodeKind] =
   this match {
     case Package(id) => this.node
     case _ => this.node.container.kind.packageNode
   }*/
}

case object TypeVariable extends JavaNodeKind{
  def canContain(k : NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractKinds(p : AbstractionPolicy) = Seq()
}

object JavaNodeKind {

  //import AccessGraph.dummyId
  /*def packageKind = Package(dummyId)
  def interface = Interface(dummyId, None)*/
  def classKind = Class
  //fix for accessing the field in java
  def interfaceKind = Interface

  def field = Field
  def constructor = Constructor
  def abstractMethod = AbstractMethod
  def method = Method

  def primitive = Primitive
  def typeVariable = TypeVariable

  def noType = NoType

  val list = Seq[NodeKind](Package, Interface,
    Class, Constructor, Method, /*ConstructorMethod,*/
    Field, AbstractMethod, Literal, Primitive)
}

case class NamedTypeHolder(typ : NamedType) extends TypeHolder{

  def redirectUses(oldUsee : NodeId,
                   newUsee: AGNode) : TypeHolder=
  NamedTypeHolder(typ.redirectUses(oldUsee, newUsee))

  def redirectContravariantUses(oldUsee : NodeId, newUsee: AGNode) =
    redirectUses(oldUsee, newUsee)

  def mkString(graph : AccessGraph) : String =  " : " + typ.toString
}

case class MethodTypeHolder(typ : Arrow[Tuple[NamedType], NamedType]) extends TypeHolder{

  def redirectUses(oldUsee : NodeId,
                   newUsee: AGNode) : TypeHolder=
    MethodTypeHolder(typ.redirectUses(oldUsee, newUsee))
  def redirectContravariantUses(oldUsee : NodeId, newUsee: AGNode) =
    MethodTypeHolder(typ.redirectContravariantUses(oldUsee, newUsee))

  def mkString(graph : AccessGraph ) : String =  " : " + typ.toString
}