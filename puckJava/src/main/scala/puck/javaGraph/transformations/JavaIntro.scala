package puck.javaGraph.transformations

import puck.graph._
import puck.graph.transformations.rules.Intro
import puck.javaGraph.MethodType
import puck.javaGraph.nodeKind._

object JavaIntro extends Intro {

  override def apply
  (graph: DependencyGraph,
   localName: String,
   kind: NodeKind,
   th: Option[Type],
   mutable: Mutability = true
    ): (ConcreteNode, DependencyGraph) = {
    val (n, g) = super.apply(graph, localName, kind, th, mutable)
    kind match {
      case Class =>
        val (ctor, g1) = apply(g, localName, Constructor,
          Some(new MethodType(Tuple(List()), NamedType(n.id))))
        (n, g1.addContains(n.id, ctor.id))

      case _ => (n, g)
    }
  }

  def accessToType
  (graph: DependencyGraph,
   localName: String,
   kind: NodeKind,
   typeNode: NodeId,
   mutable: Mutability = true
    ): (ConcreteNode, DependencyGraph) = {
    val t = kind match {
      case Field => NamedType(typeNode)
      case Method => MethodType(Tuple(List()), NamedType(typeNode))
    }

    val (cn, g) = this.apply(graph, localName, kind, Some(t))
    val (defNode, g2) = g.addConcreteNode("", Definition, None)
    (cn, g2.addContains(cn.id, defNode.id))
  }

}
