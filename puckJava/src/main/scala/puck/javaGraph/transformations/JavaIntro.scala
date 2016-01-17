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
        val (ctorDecl, _, g1) =
          intro.typedNodeWithDef(g,localName, Constructor, n.id)

        (n, g1.addContains(n.id, ctorDecl.id))

      case _ => (n, g)
    }
  }

  def nodeWithDef
  (graph: DependencyGraph,
   localName: String,
   kind: NodeKind,
   typ: Option[Type],
   mutable: Mutability
    ): (ConcreteNode, ConcreteNode, DependencyGraph) = {

    val (cn, g) = this.apply(graph, localName, kind)
    val (defNode, g2) = g.addConcreteNode(DependencyGraph.anonymousName, Definition)

    (cn, defNode,
      g2.setType(cn.id, typ)
      .addEdge(Contains(cn.id, defNode.id)))
  }


  def defaultConstructor
  ( g : DependencyGraph,
    typeNode : NodeId) : LoggedTry[(ConcreteNode, DependencyGraph)] = {

    val (cn, _, g2)=
      intro.nodeWithDef(g, g.getConcreteNode(typeNode).name, Constructor, Some(NamedType(typeNode)), mutable = true)
    LoggedSuccess((cn, g2.addContains(typeNode, cn.id)))
  }
}
