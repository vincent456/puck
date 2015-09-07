package puck.javaGraph.transformations

import puck.graph._
import puck.graph.transformations.rules.Intro
import puck.javaGraph.nodeKind._

object JavaIntro extends Intro {

  override def apply
  (graph: DependencyGraph,
   localName: String,
   kind: NodeKind,
   mutable: Mutability = true
    ): (ConcreteNode, DependencyGraph) = {
    val (n, g) = super.apply(graph, localName, kind, mutable)
    kind match {
      case Class =>
        val (ctor, g1) = apply(g, localName, Constructor)
        (n, g1.addContains(n.id, ctor.id)
              .setType(ctor.id, Some(NamedType(n.id))))

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

    val (cn, g) = this.apply(graph, localName, kind)
    val (defNode, g2) = g.addConcreteNode(DependencyGraph.anonymousName, Definition)

    (cn, g2.setType(cn.id, Some(NamedType(typeNode)))
      .addDef(cn.id, defNode.id))
  }

}
