package puck.graph.transformations.rules

import puck.{graph, PuckError}
import puck.graph._

abstract class Intro {
  intro =>

  def initializer
  ( graph : DependencyGraph,
    typeInitialized : NodeId
    ) : (NodeId, DependencyGraph) = {
    val initializedContent =
      graph content typeInitialized filter {
        nid =>
          val n = graph getNode nid
          n.kind.isWritable && n.hasDefinitionIn(graph)
      }

    import graph.nodeKindKnowledge.{writeType, initializerKind}
    val returnType = writeType(graph)

    val (cnDecl, defNode, g) =
      intro.nodeWithDef(graph, "init", initializerKind, Some(returnType), isMutable)

    val g2 =
      if(initializedContent.nonEmpty &&
        ! g.uses(typeInitialized,typeInitialized) )
        g.addUses(typeInitialized,typeInitialized)
      else g

    val ctors = graph content typeInitialized filter (graph.kindType(_) == TypeConstructor)

    val g3 = ctors.foldLeft(g2.setRole(cnDecl.id, Some(Initializer(typeInitialized)))){
      case (g0, ctor) =>
        val ctorDef = g definitionOf_! ctor
        g0.addUses(ctorDef, cnDecl.id)
          .addUsesDependency((typeInitialized,typeInitialized), (ctorDef, cnDecl.id))

    }
    (cnDecl.id,
      initializedContent.foldLeft(g3.addContains(typeInitialized, cnDecl.id)){
        (g, ic) =>
          val g1 = g.addUses(defNode.id, ic, Some(Write))
                    .addUsesDependency((typeInitialized,typeInitialized), (defNode.id, ic))

          val icDef = g definitionOf_! ic

          val g2 = g.usedBy(icDef).foldLeft(g1){
            (g0, usedByIcDef) =>
              g0.getUsesEdge_!(icDef, usedByIcDef).changeSource(g0, defNode.id)
          }
          val (_, g3) = g2.removeEdge(ContainsDef(ic, icDef)).removeNode(icDef)

          g3
      })

  }



  def apply
  ( graph : DependencyGraph,
    localName : String,
    kind : NodeKind,
    mutable : Mutability = true
    ) : (ConcreteNode, DependencyGraph) =
    graph.addConcreteNode(localName, kind, mutable)

  def typedNodeWithDef
  (graph : DependencyGraph,
   localName: String,
   kind : NodeKind,
   typeNode : NodeId,
   mutable : Mutability = true
    )  : (ConcreteNode, ConcreteNode, DependencyGraph) =
    nodeWithDef(graph, localName, kind, Some(NamedType(typeNode)), mutable)

  def nodeWithDef
  (graph : DependencyGraph,
   localName: String,
   kind : NodeKind,
   typ : Option[Type],
   mutable : Mutability
    )  : (ConcreteNode, ConcreteNode, DependencyGraph)

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

        val (delegateDecl, delegateDef, g1) =
          typedNodeWithDef(g, delegateName, kind, typeNode)

        val newTypeUse = Uses(delegateDecl.id, typeNode)

        val tmContainerKind = g.getConcreteNode(tmContainer).kind
        if(tmContainerKind canContain kind)
          LoggedSuccess(
            (newTypeUse,
              g1.addContains(tmContainer, delegateDecl.id)
                .addEdge(newTypeUse) //type field
                .addEdge(Uses(delegateDef.id, constructorId))))
        //.addEdge(Uses(tmContainer, delegateDecl.id, Some(Write)))
        else {
          val msg =s"$tmContainerKind cannot contain $kind"
          LoggedError(new PuckError(msg), msg)
        }

    }
  }

  def addUsesAndSelfDependency
  (g : DependencyGraph,
    user : NodeId,
    used : NodeId) : DependencyGraph =
    (g.kindType(g.container_!(user)), g.kindType(used)) match {
      case (InstanceValueDecl, InstanceValueDecl)
      if g.hostTypeDecl(user) == g.hostTypeDecl(used) =>
        val cter = g.hostTypeDecl(user)
        val g1 =
          if (Uses(cter, cter) existsIn g) g
          else g.addUses(cter, cter)

        g1.addUses(user, used)
          .addUsesDependency((cter, cter), (user,used))
      case _ => g.addUses(user, used)

  }
}
