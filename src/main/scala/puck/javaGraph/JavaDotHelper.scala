package puck.javaGraph

import puck.graph._
import puck.graph.io.{VisibilitySet, DotHelper}
import puck.javaGraph.nodeKind._


/**
 * Created by lorilan on 29/10/14.
 */


object JavaDotHelper extends DotHelper{

  override def isDotSubgraph(n: DGNode): Boolean =
    n.kind == Package

  override def namePrefix(n: DGNode): String =  n.kind match {
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
          val kind = graph.getConcreteNode(n).kind
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

  override def isDotClass(n : DGNode): Boolean = n.kind match {
      case Class | Interface => true;
      case _ => false
    }

  override def fillColor(n: DGNode): String = {
    def aux(cn : ConcreteNode) : String = cn.kind match {
        case Package => "#FF9933" //Orange
        case Interface => "#FFFF99" // Light yellow
        case Class | Constructor => "#FFFF33" //Yellow
        case Method | Field => "#FFFFFF" //White
        case Literal => "#CCFFCC" //Very Light green
        case _ => throw new Error("Unknown JavaNodeKind")
      }
    n mapConcrete(aux, "#00FF00")
  }
}