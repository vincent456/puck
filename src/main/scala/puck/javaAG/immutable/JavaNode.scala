package puck.javaAG.immutable

import puck.graph.immutable.AccessGraph.{NodeId, Mutability}
import puck.graph.immutable.{AccessGraph, AGNodeBuilder, AGNode}
import puck.javaAG.immutable.nodeKind.{JavaRoot, JavaNodeKind}

/**
 * Created by lorilan on 29/10/14.
 */

object JavaNode extends AGNodeBuilder[JavaNodeKind] {
  def apply(graph : AccessGraph[JavaNodeKind],
            id : NodeId[JavaNodeKind],
            name : String,
            kind : JavaNodeKind,
            isMutable : Mutability) : JavaNode =
    new JavaNode(graph, id, name, kind, isMutable)

  def rootKind : JavaNodeKind = JavaRoot(AccessGraph.rootId)
  def kinds : Seq[JavaNodeKind] = JavaNodeKind.list
}

class JavaNode
( graph : AccessGraph[JavaNodeKind],
  id : NodeId[JavaNodeKind],
  name : String,
  override val kind : JavaNodeKind,
  isMutable : Mutability)
  extends AGNode[JavaNodeKind](graph, id, name, kind, isMutable){

}
