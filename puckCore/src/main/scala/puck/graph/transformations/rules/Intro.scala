package puck.graph.transformations.rules

import puck.{graph, PuckError}
import puck.graph._

abstract class Intro {

  def apply // real intro, other methods are implementation of the abstract rule
  ( graph : DependencyGraph,
    localName : String,
    kind : NodeKind,
    mutable : Mutability = true
    ) : (ConcreteNode, DependencyGraph) =
    graph.addConcreteNode(localName, kind, mutable)

  def accessToType
  (graph : DependencyGraph,
   localName: String,
   kind : NodeKind,
   typeNode : NodeId,
   mutable : Mutability = true
    )  : (ConcreteNode, DependencyGraph)


  def parameter
  ( g : DependencyGraph,
    typeNode : NodeId,
    typeMemberDecl : NodeId
    ) : LoggedTry[(ConcreteNode, DependencyGraph)] = {

    val newTypeUsedNode = g.getConcreteNode(typeNode)
    val paramKind = g.nodeKindKnowledge.kindOfKindType(Parameter).head
    val (pNode, g1) = g.addConcreteNode(newTypeUsedNode.name.toLowerCase, paramKind)


    g1.getDefaultConstructorOfType(typeNode) match {
      case None => LoggedError(new PuckError(s"no default constructor for $typeNode"))
      case Some(cid) =>
        LoggedSuccess {
          val g2 = g1.addParam(typeMemberDecl, pNode.id)
            .setType(pNode.id, Some(NamedType(typeNode)))

          (pNode,
            g1.usersOf(typeMemberDecl).foldLeft(g2) {
              (g0, userOfUser) =>
                g0.addEdge(Uses(userOfUser, cid))
            })
        }
    }


  }

  def typeMember
  (g : DependencyGraph,
   typeNode : NodeId,
   tmContainer : NodeId,
   kind : NodeKind
    ) : LoggedTry[(DGUses, DependencyGraph)] = {
    g.getDefaultConstructorOfType(typeNode) match {
      case None => LoggedError(new PuckError(s"no default constructor for $typeNode"))
      case Some(constructorId) =>

        val delegateName = s"${g.getConcreteNode(typeNode).name.toLowerCase}_delegate"

        val (delegate, g1) = accessToType(g, delegateName, kind, typeNode)

        val newTypeUse = Uses(delegate.id, typeNode)

        val tmContainerKind = g.getConcreteNode(tmContainer).kind
        if(tmContainerKind canContain kind)
          LoggedSuccess(
            (newTypeUse,
              g1.addContains(tmContainer, delegate.id)
                .addEdge(newTypeUse) //type field
                .addEdge(Uses(g1 definition_! delegate.id, constructorId))))
        //.addEdge(Uses(tmContainer, delegate.id, Some(Write)))
        else {
          val msg =s"$tmContainerKind cannot contain $kind"
          LoggedError(new PuckError(msg), msg)
        }

    }
  }
}
