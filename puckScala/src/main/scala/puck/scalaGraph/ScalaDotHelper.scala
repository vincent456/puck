package puck.scalaGraph

import puck.graph.{NodeId, ConcreteNode, DependencyGraph, DGNode}
import puck.graph.io.{VisibilitySet, DotHelper}
import nodeKind._
import VisibilitySet._

object ScalaDotHelper extends DotHelper {
  override def isDotSubgraph(n: DGNode): Boolean =
    n.kind == Package

  override def namePrefix(n: DGNode): String =  n.kind match {
    case Package => "&lt;&lt;package&gt;&gt; "
    case Trait => "&lt;&lt;trait&gt;&gt; "
    case Object
      | PackageObject => "&lt;&lt;object&gt;&gt; "
    case _ => ""
  }

  override def splitDotClassContent(graph : DependencyGraph, n: NodeId, visibility : VisibilitySet.T) = {
    graph.content(n).foldLeft( (Seq[NodeId](), Seq[NodeId](), Seq[NodeId](), Seq[NodeId]()) ){
      ( lists : (Seq[NodeId], Seq[NodeId], Seq[NodeId] , Seq[NodeId]), n : NodeId ) =>
        if(visibility.isHidden(n)) lists
        else {
          val (fds, cts, mts, cls) = lists
          val kind = graph.getConcreteNode(n).kind
          kind match {
            case Trait | Class | Object | PackageObject => (fds, cts, mts, n +: cls)
            case Var | Val => (n +: fds, cts, mts, cls)
            case Def => (fds, cts, n +: mts, cls)

            case _ => throw new Error(kind + " : wrong NodeKind contained by a class")
          }
        }
    }
  }

  override def isDotClass(n : DGNode): Boolean = n.kind match {
    case Class | Trait | Object | PackageObject => true
    case _ => false
  }

  override def fillColor(n: DGNode): String = {
    def aux(cn : ConcreteNode) : String = cn.kind match {
      case Package => "#FF9933" //Orange
      case Trait => "#FFFF99" // Light yellow
      case Class => "#FFFF33" //Yellow
      case Object | PackageObject => "#FFFF22" //Yellow
      case Def | Var | Val => "#FFFFFF" //White
      case _ => throw new Error("Unknown ScalaNodeKind")
    }
    n mapConcrete(aux, "#00FF00")
  }
}
