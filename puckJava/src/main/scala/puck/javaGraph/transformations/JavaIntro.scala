package puck.javaGraph.transformations

import puck.graph._
import puck.graph.transformations.rules.Intro
import puck.javaGraph.nodeKind._

object JavaIntro extends Intro {
  intro =>
  override def apply
  (graph: DependencyGraph,
   localName: String,
   kind: NodeKind,
   mutable: Mutability = true
    ): (ConcreteNode, DependencyGraph) = {
    val (n, g) = super.apply(graph, localName, kind, mutable)
    kind match {
      case Class =>
        val (ctor, g1) =
          intro.typedNodeWithDef(g,localName, Constructor, n.id)

        (n, g1.addContains(n.id, ctor.id))

      case _ => (n, g)
    }
  }

  def typedNodeWithDef
  (graph: DependencyGraph,
   localName: String,
   kind: NodeKind,
   typ: Type,
   mutable: Mutability
    ): (ConcreteNode, DependencyGraph) = {

    val (cn, g) = this.apply(graph, localName, kind)
    val (defNode, g2) = g.addConcreteNode(DependencyGraph.anonymousName, Definition)

    (cn, g2.setType(cn.id, Some(typ))
      .addDef(cn.id, defNode.id))
  }

}
