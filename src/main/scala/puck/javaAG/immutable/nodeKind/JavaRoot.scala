package puck.javaAG.immutable.nodeKind

import puck.graph.immutable.AGRoot
import puck.graph.immutable.AccessGraph.NodeId

/**
 * Created by lorilan on 31/07/14.
 */
case class JavaRoot(node : NodeId[JavaNodeKind]) extends JavaNodeKind with AGRoot[JavaNodeKind]{
  override val toString = "JavaRoot"
  def create(node : NodeId[JavaNodeKind]) = JavaRoot(node)
  var program : AST.Program = _
}
