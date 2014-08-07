package puck.javaAG.nodeKind

import puck.graph._
import puck.javaAG.{JavaNamedType, MethodType}

/**
 * Created by lorilan on 31/07/14.
 */
abstract class JavaNodeKind extends NodeKind[JavaNodeKind]{
  def createDecl(n : AGNode[JavaNodeKind]) {
    throw new Error("do not know how to create declaration for" + getClass)
  }
}

object JavaNodeKind {

  def typedKind[S <: Type[JavaNodeKind, S], T <: HasType[JavaNodeKind, S]]( ctr : () => T, t: S) = {
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

  def constructor(t : MethodType.T) = typedKind ( () => Constructor(), t )
  def method(t : MethodType.T) = typedKind ( () => Method(), t )
  def field(t : NamedType[JavaNodeKind]) = typedKind ( () => Field(), t )
  def abstractMethod(t : MethodType.T) = typedKind ( () => AbstractMethod(), t )
  def literal(t : NamedType[JavaNodeKind]) = typedKind ( () => Literal(), t )


  val list = List[JavaNodeKind](Package(), Interface(),
    Class(), Constructor(),
    Method(), Field(), AbstractMethod(), Literal(), Primitive())



}
