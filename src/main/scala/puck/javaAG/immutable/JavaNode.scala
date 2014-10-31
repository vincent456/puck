package puck.javaAG.immutable

import puck.graph.immutable.AccessGraph.{NodeId, Mutability}
import puck.graph.immutable.io.DotHelper
import puck.graph.immutable.{AccessGraph, AGNodeBuilder, AGNode}
import puck.javaAG.immutable.nodeKind._

/**
 * Created by lorilan on 29/10/14.
 */

object JavaNode extends AGNodeBuilder[JavaNodeKind] with DotHelper[JavaNodeKind]{
  def apply(graph : AccessGraph[JavaNodeKind],
            id : NodeId[JavaNodeKind],
            name : String,
            kind : JavaNodeKind,
            isMutable : Mutability) : JavaNode =
    new JavaNode(graph, id, name, kind, isMutable)

  def rootKind : JavaNodeKind = JavaRoot(AccessGraph.rootId)
  def kinds : Seq[JavaNodeKind] = JavaNodeKind.list

  override def isDotSubgraph(k: JavaNodeKind): Boolean = k.isInstanceOf[Package]

  override def namePrefix(k: JavaNodeKind): String =  k match {
    case Package(_) => "&lt;&lt;package&gt;&gt; "
    case Interface(_,_) => "&lt;&lt;interface&gt;&gt; "
    case _ => ""
  }

  override def splitDotClassContent(graph : AccessGraph[JavaNodeKind], n: NodeId[JavaNodeKind]) = {
    graph.getNode(n).content.foldLeft( (Seq[NodeId[JavaNodeKind]](), Seq[NodeId[JavaNodeKind]](), Seq[NodeId[JavaNodeKind]](), Seq[NodeId[JavaNodeKind]]()) ){
      ( lists : (Seq[NodeId[JavaNodeKind]], Seq[NodeId[JavaNodeKind]], Seq[NodeId[JavaNodeKind]] , Seq[NodeId[JavaNodeKind]]), n : NodeId[JavaNodeKind] ) =>
        val (fds, cts, mts, cls) = lists
        val kind = graph.getNode(n).kind
        kind match {
          case Interface(_,_) | Class(_,_) => (fds, cts, mts, n+:cls)
          case Field(_,_,_) => (n+:fds, cts, mts, cls)
          case Constructor(_,_,_) => (fds, n+:cts, mts, cls)
          case AbstractMethod(_,_,_)
               | Method(_,_,_) => (fds, cts, n+:mts, cls)

          case _ => throw new Error(kind + " : wrong NodeKind contained by a class" )
        }
    }
  }

  override def isDotClass(k: JavaNodeKind): Boolean = k match { case Class(_,_) | Interface(_,_) => true; case _ => false}

  override def fillColor(k: JavaNodeKind): String = k match {
    case Package(_) => "#FF9933" //Orange
    case Interface(_,_) => "#FFFF99" // Light yellow
    case Class(_,_) | Constructor(_,_,_) => "#FFFF33" //Yellow
    case Method(_,_,_) | Field(_,_,_) => "#FFFFFF" //White
    case Literal(_,_) => "#CCFFCC" //Very Light green
    case _ => throw new Error("Unknown JavaNodeKind")
  }
}

class JavaNode
( graph : AccessGraph[JavaNodeKind],
  id : NodeId[JavaNodeKind],
  name : String,
  override val kind : JavaNodeKind,
  isMutable : Mutability)
  extends AGNode[JavaNodeKind](graph, id, name, kind, isMutable){

}
