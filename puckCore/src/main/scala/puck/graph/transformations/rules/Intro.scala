/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.graph.transformations.rules
import puck.graph._
import puck.util.LoggedEither._
import ShowDG._

import scalaz.std.list._

sealed trait CreateVarStrategy {
  def apply ( g : DependencyGraph, oldTu : NodeIdP, newTu : NodeId, tmUses : Set[NodeIdP] ) : LoggedTG
}
case object CreateParameter extends CreateVarStrategy {
  def apply ( g : DependencyGraph, oldTu : NodeIdP, newTu : NodeId, tmUses : Set[NodeIdP] ) : LoggedTG =
    Intro.param(g,oldTu, newTu, tmUses)

}
case class CreateTypeMember(kind : NodeKind) extends CreateVarStrategy {
  def apply ( g : DependencyGraph, oldTu : NodeIdP, newTu : NodeId, tmUses : Set[NodeIdP] ) : LoggedTG =
    Intro.typeMember(g, oldTu, newTu, tmUses, kind)
}

object Intro {

  def param
  (g : DependencyGraph,
   oldTypeUse : NodeIdP,
   newTypeUsed : NodeId,
   tmUses : Set[NodeIdP]
  ): LoggedTG ={

    val usesByUser = tmUses.groupBy(_.user)
    //introduce one parameter by user even with several uses
    //these were all previously this use so it makes sens to keep one reference
    val tmUsesStr = tmUses.map(u => (g,u).shows).mkString("[",", ","]")
    usesByUser.toList.foldLoggedEither(g.comment(s"createParam(g,${(g, oldTypeUse).shows}, ${(g,newTypeUsed).shows}, $tmUsesStr)")) {
      case (g0, (impl, typeMemberUses)) =>
        val user = g0.getConcreteNode(impl)

        assert(g0.getConcreteNode(impl).kind.kindType == ValueDef)

        val decl = g0.getConcreteNode(g0.container_!(impl))

        paramOfTypeAndSetTypeDependency(g0, decl.id,
          newTypeUsed, oldTypeUse, typeMemberUses) map (_._2)

    }
  }

  def paramOfTypeAndSetTypeDependency
  (g: DependencyGraph,
   declId : NodeId,
   pType : NodeId,
   oldTypeUse : NodeIdP,
   tmUses : Set[NodeIdP]
  ) : LoggedTry[(ConcreteNode, DependencyGraph)]  = {
    import g.nodeKindKnowledge.intro
    intro.parameter(g, pType, declId) map {
      case (pNode, g2) =>
        val newTypeUse = (pNode.id, pType)
        (pNode, g2.changeTypeUseForTypeMemberUseSet(oldTypeUse, newTypeUse, tmUses))
    }
  }

  def typeMember
  (g : DependencyGraph,
   oldTypeUse : NodeIdP,
   newTypeUsed : NodeId,
   tmUses : Set[NodeIdP],
   kind : NodeKind
  ): LoggedTG ={

    val tmContainer =
      if (oldTypeUse.selfUse) oldTypeUse.user
      else g.container(oldTypeUse.user).get

    // assert forall user in tmUses, container(user) = tmContainer
    import g.nodeKindKnowledge.intro
    for {
      ug <- intro.typeMember(g, newTypeUsed, tmContainer, kind)
    } yield {
      val (newTypeUse, g2) = ug
      val delegateId = newTypeUse.user

      tmUses.foldLeft(g2) {
        case (g0, typeMemberUse) =>
          intro.addUsesAndSelfDependency(
            g0.changeTypeUseOfTypeMemberUse(oldTypeUse, newTypeUse, typeMemberUse),
            typeMemberUse.user, delegateId) // replace this.m by this.delegate.m
      }
    }
  }
}

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

    import graph.nodeKindKnowledge.{unitType, initializerKind}
    val returnType = unitType(graph)

    val (cnDecl, defNode, g) =
      intro.nodeWithDef(graph, "init", initializerKind, returnType)

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
          .addBinding((typeInitialized,typeInitialized), (ctorDef, cnDecl.id))

    }
    (cnDecl.id,
      initializedContent.foldLeft(g3.addContains(typeInitialized, cnDecl.id)){
        (g, ic) =>
          val g1 = g.addUses(defNode.id, ic)
                    .addBinding((typeInitialized,typeInitialized), (defNode.id, ic))
                      .addAccessKind(((typeInitialized,typeInitialized), (defNode.id, ic)), Write)

          val icDef = g definitionOf_! ic

          val g2 = g.usedByExcludingTypeUse(icDef).foldLeft(g1){
            (g0, usedByIcDef) =>
              Uses(icDef, usedByIcDef).changeSource(g0, defNode.id)
          }
          val (_, g3) = g2.removeEdge(Contains(ic, icDef)).removeNode(icDef)

          g3
      })

  }



  def apply
  ( graph : DependencyGraph,
    localName : String,
    kind : NodeKind
    ) : (ConcreteNode, DependencyGraph) =
    graph.comment(s"Intro(g, $localName, $kind)").addConcreteNode(localName, kind)

  def typedNodeWithDef
  (graph : DependencyGraph,
   localName: String,
   kind : NodeKind,
   typeNode : NodeId
  )  : (ConcreteNode, ConcreteNode, DependencyGraph) =
    nodeWithDef(graph, localName, kind, NamedType(typeNode))

  def nodeWithDef
  (graph : DependencyGraph,
   localName: String,
   kind : NodeKind,
   typ : Type)  : (ConcreteNode, ConcreteNode, DependencyGraph)

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

    g.logComment(s"Intro parameter typed ${(g,NamedType(typeNode)).shows} " +
        s"to ${(g, typeMemberDecl).shows}").flatMap {
      g0 =>
        val (pNode, g1) = g0.addConcreteNode(newTypeUsedNode.name.toLowerCase, paramKind)
        LoggedSuccess((pNode, g1.addContains(typeMemberDecl, pNode.id)
          .addType(pNode.id, NamedType(typeNode))))
    }
  }

  /*def parameter
  ( g : DependencyGraph,
    typeNode : NodeId,
    typeMemberDecl : NodeId
  ) : LoggedTry[(ConcreteNode, DependencyGraph)] = {

    val newTypeUsedNode = g.getConcreteNode(typeNode)
    val paramKind = g.nodeKindKnowledge.kindOfKindType(Parameter).head

    g.logComment(s"Intro parameter typed ${(g,NamedType(typeNode)).shows} " +
      s"to ${(g, typeMemberDecl).shows}").flatMap {
      g0 =>
        val (pNode, g1) = g0.addConcreteNode(newTypeUsedNode.name.toLowerCase, paramKind)


        def onSuccess(cid : NodeId, g : DependencyGraph) : (ConcreteNode, DependencyGraph) = {
          val g2 = g.addContains(typeMemberDecl, pNode.id)
            .addType(pNode.id, NamedType(typeNode))

          (pNode,
            g.usersOfExcludingTypeUse(typeMemberDecl).foldLeft(g2) {
              (g0, userOfUser) =>
                g0.addEdge(Uses(userOfUser, cid))
            })
        }

        g1.getDefaultConstructorOfType(typeNode) match {
          case None =>
            intro.defaultConstructor(g1, typeNode) map {
              case (cn, g2) => onSuccess(cn.id, g2)
            }
          case Some(cid) =>
            LoggedSuccess(onSuccess(cid, g1))
        }
    }

  }*/

  def typeMember
  (graph : DependencyGraph,
   typeNode : NodeId,
   tmContainer : NodeId,
   kind : NodeKind
    ) : LoggedTry[(NodeIdP, DependencyGraph)] = {
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

        val newTypeUse = (delegateDecl.id, typeNode)

        val tmContainerKind = g0.getConcreteNode(tmContainer).kind
        if(tmContainerKind canContain kind) LoggedSuccess {
            (newTypeUse,
              g1.addContains(tmContainer, delegateDecl.id)
                .addEdge(Uses(delegateDef.id, constructorId))
                .addTypeConstraint(Sub(TypeOf(constructorId), TypeOf(delegateDecl.id))))
        }
        else LoggedError(s"$tmContainerKind cannot contain $kind")

    }
  }

  def addUsesAndSelfDependency
  (g : DependencyGraph,
    user : NodeId,
    used : NodeId) : DependencyGraph =
    (g.kindType(g.container_!(user)), g.kindType(used)) match {
      case (InstanceValue, InstanceValue)
      if g.hostTypeDecl(user) == g.hostTypeDecl(used) =>
        val cter = g.hostTypeDecl(user)
        val g1 =
          if (g exists Uses(cter, cter)) g
          else g.addUses(cter, cter)

        g1.addUses(user, used)
          .addBinding((cter, cter), (user,used))
      case _ => g.addUses(user, used)

  }

  def constructorByCopy
  ( g : DependencyGraph,
    classId : NodeId) : LoggedTry[(ConcreteNode, DependencyGraph)]
}
