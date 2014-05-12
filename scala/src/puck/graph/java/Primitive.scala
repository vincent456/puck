package puck.graph.java

import puck.graph.{NodeKind, AccessGraph, StatelessAGNode, NamedType}

/**
 * Created by lorilan on 05/05/14.
 */
case class Primitive private () extends NodeKind

object Primitive {

  val void = NamedType("@primitive.void", -1)
  val boolean = NamedType("@primitive.boolean", -2)
  val byte = NamedType("@primitive.byte", -3)
  val char = NamedType("@primitive.char", -4)
  val double = NamedType("@primitive.double", -5)
  val float = NamedType("@primitive.float", -6)
  val int = NamedType("@primitive.int", -7)
  val long = NamedType("@primitive.long", -8)
  val short = NamedType("@primitive.short", -9)

  // not primitive but ...
  val string = NamedType("java.lang.String", -10)

  private def makePrimitiveNode (t:NamedType, g: AccessGraph) = {
    val n = new StatelessAGNode(g, t.id, t.name, Primitive(), Some(t))
    /*to prevent attach this node to the AG root */
    n.container= Some(n)
    n
  }

  def voidNode(g : AccessGraph) = makePrimitiveNode(void, g)
  def booleanNode(g : AccessGraph) = makePrimitiveNode(boolean, g)
  def byteNode(g : AccessGraph) = makePrimitiveNode(byte, g)
  def charNode(g : AccessGraph) = makePrimitiveNode(char, g)
  def doubleNode(g : AccessGraph) = makePrimitiveNode(double, g)
  def floatNode(g : AccessGraph) = makePrimitiveNode(float, g)
  def intNode(g : AccessGraph) = makePrimitiveNode(int, g)
  def longNode(g : AccessGraph) = makePrimitiveNode(long, g)
  def shortNode(g : AccessGraph) = makePrimitiveNode(short, g)

  def stringNode(g : AccessGraph) = makePrimitiveNode(string, g)
}
