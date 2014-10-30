package puck.javaAG.mutable.nodeKind

import puck.graph.mutable.AGRoot

/**
 * Created by lorilan on 31/07/14.
 */
case class JavaRoot() extends JavaNodeKind with AGRoot[JavaNodeKind]{
  override val toString = "JavaRoot"
  def create() = JavaRoot()
  var program : AST.Program = _
}
