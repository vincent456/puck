package puck.javaAG.nodeKind

import puck.graph.{HasType, Type, AGNode, NodeKind}
import puck.javaAG.{JavaType, MethodType}

/**
 * Created by lorilan on 31/07/14.
 */
abstract class JavaNodeKind extends NodeKind[JavaNodeKind]{
  def createDecl(n : AGNode[JavaNodeKind]) {
    throw new Error("do not know how to create declaration for" + getClass)
  }
}

object JavaNodeKind {

  def typedKind[S<:Type, T<:HasType[S]]( ctr : () => T, t: S) = {
    val k = ctr ()
    k.`type` = t
    k
  }

  def `package` = Package()

  def interface = Interface()
  def `class` = Class()
  //fix for accessing the field in java
  def interfaceKind = interface
  def classKind = `class`

  def constructor(t : MethodType) = typedKind ( () => Constructor(), t )
  def method(t : MethodType) = typedKind ( () => Method(), t )
  def field(t : JavaType) = typedKind ( () => Field(), t )
  def abstractMethod(t : MethodType) = typedKind ( () => AbstractMethod(), t )
  def literal(t : JavaType) = typedKind ( () => Literal(), t )


  val list = List[JavaNodeKind](Package(), Interface(),
    Class(), Constructor(),
    Method(), Field(), AbstractMethod(), Literal(), Primitive())



}
