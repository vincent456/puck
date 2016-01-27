package puck.javaGraph

import puck.graph.{ConcreteNode, NodeId, DependencyGraph, DGNode}
import puck.graph.io.{VisibilitySet, DotHelper}
import puck.javaGraph.nodeKind._
import VisibilitySet._

object JavaDotHelper extends DotHelper{

  override def isDotSubgraph(n: DGNode): Boolean =
    n.kind == Package

  override def namePrefix(n: DGNode): String =  n.kind match {
      case Package => "&lt;&lt;package&gt;&gt; "
      case Interface => "&lt;&lt;interface&gt;&gt; "
      case _ => ""
    }

  override def splitByKind(graph : DependencyGraph, ns: Seq[NodeId]) = {
    val init : Seq[Seq[NodeId]] = Seq(Seq(), Seq(), Seq(), Seq(), Seq())
    ns.foldLeft( init ){
      case (Seq(fds, cts, mts, cls, tvs), n) =>
          val kind = graph.getConcreteNode(n).kind
          kind match {
            case Interface | Class | InnerClass | InnerInterface => Seq(fds, cts, mts, n +: cls, tvs)

            case Constructor => Seq(fds, n +: cts, mts, cls, tvs)

            case Field
            | StaticField => Seq(n +: fds, cts, mts, cls, tvs)

            case _ : MethodKind
            | StaticMethod => Seq(fds, cts, n +: mts, cls, tvs)

            case TypeVariable => Seq(fds, cts, mts, cls, n +: tvs)

            case _ => throw new Error(kind + " : wrong NodeKind contained by a class")
          }
        }

  }

  override def isDotClass(n : DGNode): Boolean = n.kind match {
      case Class | Interface | Primitive => true
      case _ => false
    }

  override def fillColor(n: DGNode): String = {
    def aux(cn : ConcreteNode) : String = cn.kind match {
        case Package => "#FF9933" //Orange
        case Interface => "#FFFF99" // Light yellow
        case Class | Constructor => "#FFFF33" //Yellow
        case Method | Field => "#FFFFFF" //White
        case Literal => "#CCFFCC" //Very Light green
        case Primitive => "#FFFFFF"
        case _ => throw new Error("Unknown JavaNodeKind")
      }
    n mapConcrete(aux, "#00FF00")
  }
}