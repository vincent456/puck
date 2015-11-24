package puck.graph.transformations.rules

import puck.{graph, PuckError}
import puck.graph._
import ShowDG._

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
    graph.comment(s"Intro(g, $localName, $kind)").addConcreteNode(localName, kind, mutable)

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

  def defaultConstructor
  ( g : DependencyGraph,
    typeNode : NodeId) : LoggedTry[(ConcreteNode, DependencyGraph)]

  def parameter
  ( g : DependencyGraph,
    typeNode : NodeId,
    typeMemberDecl : NodeId
    ) : LoggedTry[(ConcreteNode, DependencyGraph)] = {

    val newTypeUsedNode = g.getConcreteNode(typeNode)
    val paramKind = g.nodeKindKnowledge.kindOfKindType(Parameter).head
    val (pNode, g1) = g.addConcreteNode(newTypeUsedNode.name.toLowerCase, paramKind)


    def onSuccess(cid : NodeId, g : DependencyGraph) : (ConcreteNode, DependencyGraph) = {
      val g2 = g.addParam(typeMemberDecl, pNode.id)
        .setType(pNode.id, Some(NamedType(typeNode)))

      (pNode,
        g.usersOf(typeMemberDecl).foldLeft(g2) {
          (g0, userOfUser) =>
            g0.addEdge(Uses(userOfUser, cid))
        })
    }

    g1.getDefaultConstructorOfType(typeNode) match {
      case None =>
        intro.defaultConstructor(g1, typeNode) map {
          case (cn, g2) => onSuccess(cn.id, g2)
        }
//        LoggedError(s"no default constructor for $typeNode")
      case Some(cid) =>
        LoggedSuccess(onSuccess(cid, g1))
    }


  }

  def typeMember
  (graph : DependencyGraph,
   typeNode : NodeId,
   tmContainer : NodeId,
   kind : NodeKind
    ) : LoggedTry[(DGUses, DependencyGraph)] = {
    val g = graph.comment(s"Intro.typeMember(g, ${(graph, typeNode).shows}, ${(graph,tmContainer).shows}, $kind)")

    val ltCid : LoggedTry[(NodeId, DependencyGraph)] =
    g.getDefaultConstructorOfType(typeNode) match {
      case None => intro.defaultConstructor(g, typeNode) map {
        case (cn, g) => (cn.id, g)
      }
        //LoggedError(s"no default constructor for $typeNode")
      case Some(constructorId) => LoggedSuccess((constructorId, g))
    }

    ltCid flatMap {
      case (constructorId, g0) =>
        val delegateName = s"${g0.getConcreteNode(typeNode).name.toLowerCase}_delegate"

        val (delegateDecl, delegateDef, g1) =
          typedNodeWithDef(g0, delegateName, kind, typeNode)

        val newTypeUse = Uses(delegateDecl.id, typeNode)

        val tmContainerKind = g0.getConcreteNode(tmContainer).kind
        if(tmContainerKind canContain kind)
          LoggedSuccess(
            (newTypeUse,
              g1.addContains(tmContainer, delegateDecl.id)
                .addEdge(Uses(delegateDef.id, constructorId))))
        else
          LoggedError(s"$tmContainerKind cannot contain $kind")

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
