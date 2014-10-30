package puck.javaAG.immutable.nodeKind

import puck.graph.immutable.{AccessGraph, Tuple, NamedType, NodeKind}
import puck.javaAG.immutable.{MethodType, JavaNamedType}


/**
 * Created by lorilan on 31/07/14.
 */
abstract class JavaNodeKind extends NodeKind[JavaNodeKind]{
  def createDecl(prog : AST.Program,
                 graph : AccessGraph[JavaNodeKind]) : AccessGraph[JavaNodeKind] = {
    throw new Error("do not know how to create declaration for" + getClass)
  }

  /*def packageNode : AGNode[JavaNodeKind] =
   this match {
     case Package(id) => this.node
     case _ => this.node.container.kind.packageNode
   }*/
}

object JavaNodeKind {

  import AccessGraph.dummyId
  def packageKind = Package(dummyId)
  def interface = Interface(dummyId, None)
  def classKind = Class(dummyId, None)
  //fix for accessing the field in java
  def interfaceKind = interface


  def constructor(t : MethodType.T) = Constructor(dummyId, t, None)
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
  val primitivePrototype = Primitive(dummyId, None)

  val list = Seq[JavaNodeKind](packageKind, interface,
    classKind, constructorPrototype, methodPrototype,
    fieldPrototype, abstractMethodPrototype , literalPrototype, primitivePrototype)




}
