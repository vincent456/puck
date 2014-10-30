package puck.javaAG.immutable.nodeKind

import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.AGError
import puck.graph.constraints.AbstractionPolicy

/**
 * Created by lorilan on 31/07/14.
 */
case class Primitive private[javaAG] (node : NodeId[JavaNodeKind],
                                      decl : Option[AST.TypeDecl]) extends TypeKind {
  override val toString = "Primitive"

  def create(node : NodeId[JavaNodeKind]) = Primitive(node, None)



  def canContain(k: JavaNodeKind) = false
  def abstractKinds(p : AbstractionPolicy) =
    throw new AGError("do not know how to abstract primitive kind")
}
