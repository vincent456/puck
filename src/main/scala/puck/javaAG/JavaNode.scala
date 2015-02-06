package puck.javaAG

import puck.graph.DependencyGraph.Mutability
import puck.graph._
import puck.graph.io.{VisibilitySet, DotHelper}
import puck.javaAG.nodeKind._


/**
 * Created by lorilan on 29/10/14.
 */

import scala.language.existentials

object JavaNode extends AGNodeBuilder with DotHelper{
  def apply(id : NodeId,
            name : String,
            kind : NodeKind,
            styp : TypeHolder,
            isMutable : Mutability,
            status : NodeStatus) : DGNode =
    new DGNode(id, name, kind, styp, isMutable, status)

  def rootKind : JavaNodeKind = JavaRoot
  def kinds : Seq[NodeKind] = JavaNodeKind.list

  override def isDotSubgraph(k: NodeKind): Boolean = k == Package

  override def namePrefix(k: NodeKind): String =  k match {
    case Package => "&lt;&lt;package&gt;&gt; "
    case Interface => "&lt;&lt;interface&gt;&gt; "
    case _ => ""
  }

  override def splitDotClassContent(graph : DependencyGraph, n: NodeId, visibility : VisibilitySet) = {
    graph.content(n).foldLeft( (Seq[NodeId](), Seq[NodeId](), Seq[NodeId](), Seq[NodeId]()) ){
      ( lists : (Seq[NodeId], Seq[NodeId], Seq[NodeId] , Seq[NodeId]), n : NodeId ) =>
        if(visibility.isHidden(n)) lists
        else {
          val (fds, cts, mts, cls) = lists
          val kind = graph.getNode(n).kind
          kind match {
            case Interface | Class => (fds, cts, mts, n +: cls)
            case Field => (n +: fds, cts, mts, cls)
            case Constructor => (fds, n +: cts, mts, cls)
            case AbstractMethod
                 | Method
                 | ConstructorMethod => (fds, cts, n +: mts, cls)

            case _ => throw new Error(kind + " : wrong NodeKind contained by a class")
          }
        }
    }
  }

  override def isDotClass(k: NodeKind): Boolean = k match { case Class | Interface => true; case _ => false}

  override def fillColor(k: NodeKind): String = k match {
    case Package => "#FF9933" //Orange
    case Interface => "#FFFF99" // Light yellow
    case Class | Constructor => "#FFFF33" //Yellow
    case Method | Field => "#FFFFFF" //White
    case Literal => "#CCFFCC" //Very Light green
    case _ => throw new Error("Unknown JavaNodeKind")
  }
}