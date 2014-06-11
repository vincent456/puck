package puck.javaAG

import puck.graph.{AGError, NodeKind, AccessGraph, StatelessAGNode}
import puck.javaAG.JavaNodeKind.{`class`}
import puck.graph.constraints.AbstractionPolicy

/**
 * Created by lorilan on 05/05/14.
 */
case class Primitive private [javaAG] () extends NodeKind {

  def canContain(k: NodeKind) = false
  def abstractKinds(p : AbstractionPolicy) =
    throw new AGError("do not know how to abstract primitive kind")
}

object Primitive {

  val void = ("@primitive.void", -1)
  val boolean = ("@primitive.boolean", -2)
  val byte = ("@primitive.byte", -3)
  val char = ("@primitive.char", -4)
  val double = ("@primitive.double", -5)
  val float = ("@primitive.float", -6)
  val int = ("@primitive.int", -7)
  val long = ("@primitive.long", -8)
  val short = ("@primitive.short", -9)

  val string = ("java.lang.String", -10)


  private def makePrimitiveNode (name_id : (String, Int), g: AccessGraph) = {
    val n = new StatelessAGNode(g, name_id._2, name_id._1, Primitive())
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

  def stringNode(g : AccessGraph) = {
    val n = new StatelessAGNode(g, string._2, string._1, `class`)
    n.container= Some(n)
    n
  }
}
