package puck.javaAG.nodeKind

import puck.graph.AGRoot

/**
 * Created by lorilan on 31/07/14.
 */
case class JavaRoot() extends JavaNodeKind with AGRoot[JavaNodeKind]{
  override val toString = "JavaRoot"
  def create() = JavaRoot()
  var program : AST.Program = _
}
