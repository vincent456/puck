package puck.javaAG.immutable.nodeKind

import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable._
import puck.javaAG.immutable.{MethodType, JavaNamedType}


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

  def noType = NoType
/*  def constructor(t : MethodType.T) = Constructor(dummyId, t, None)
  def method(t : MethodType.T) = Method(dummyId, t, None)
  def field(t : NamedType[JavaNodeKind]) = Field(dummyId, t, None)
  def abstractMethod(t : MethodType.T) = AbstractMethod(dummyId, t, None)
  def literal(t : NamedType[JavaNodeKind]) = Literal(dummyId, t )


  val dummyJavaNamedType : NamedType[JavaNodeKind] = new JavaNamedType(dummyId, "DummyJavaClass")
  val dummyTupleType = new Tuple[JavaNodeKind, NamedType[JavaNodeKind]](Seq(dummyJavaNamedType))
  val dummyMethodType = new MethodType(dummyTupleType, dummyJavaNamedType)
  val constructorPrototype = constructor(dummyMethodType)
  val methodPrototype = method(dummyMethodType)
  val fieldPrototype = field(dummyJavaNamedType)
  val abstractMethodPrototype = abstractMethod(dummyMethodType)
  val literalPrototype = literal(dummyJavaNamedType)
  val primitivePrototype = Primitive(dummyId, None)*/

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