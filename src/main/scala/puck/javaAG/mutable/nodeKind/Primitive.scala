package puck.javaAG.mutable.nodeKind

import puck.graph.AGError
import puck.graph.mutable.constraints.AbstractionPolicy

/**
 * Created by lorilan on 31/07/14.
 */
case class Primitive private[javaAG] () extends TypeKind {
  override val toString = "Primitive"

  def create() = Primitive()

  var decl : AST.TypeDecl = _

  def canContain(k: JavaNodeKind) = false
  def abstractKinds(p : AbstractionPolicy) =
    throw new AGError("do not know how to abstract primitive kind")
}
