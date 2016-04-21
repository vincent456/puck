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

import puck.PuckError
import puck.graph._
import puck.util.LoggedEither
import LoggedEither._
import puck.graph.ShowDG._

import scalaz.std.list._
import scalaz.std.set._

object Redirection {

  def redirectSourceOfInitUseInFactory
  ( g: DependencyGraph,
    ctorDecl: NodeId,
    ctorDef : NodeId,
    initializer : NodeId,
    factory : NodeId) : DependencyGraph = {

    val clazz = g.container_!(ctorDecl)
    val factoryDef = g.definitionOf_!(factory)
    val selfUse = Uses(clazz,clazz)
    val g1 = g.changeSource(g.getUsesEdge_!(ctorDef, initializer), factoryDef)
      .addUses(factoryDef, clazz)
      .removeBinding(selfUse, (ctorDef, initializer))
      .addBinding((factoryDef, clazz), (factoryDef, initializer))

    if(g1.typeMemberUsesOf(selfUse).isEmpty)
      g1.removeEdge(selfUse)
    else g1
  }

  def cl(g: DependencyGraph, u : Uses) : Set[(Uses, Uses)] = {
    val cl0 =  for {
      tu <- g.typeUsesOf(u) + u
      tmu <- g.typeMemberUsesOf(tu)
    } yield (tu, tmu)

    val cl1 = for  {
      tmu <- g.typeMemberUsesOf(u) + u
      tu <- g.typeUsesOf(tmu)
    } yield (tu, tmu)

    cl0 ++ cl1
  }


  def tmAbstraction
  ( g : DependencyGraph,
    typeAbs : NodeId,
    tmImpl : NodeId
    ) : LoggedTry[Abstraction] = {
    val log = s"tmAbstraction(g, ${(g, typeAbs).shows}, ${(g, tmImpl).shows})"
    val absSet = g.abstractions(tmImpl).filter { abs =>
      abs.nodes.forall(g.contains(typeAbs,_))
    }
    if(absSet.size != 1)
      LoggedError(log + s"one abstraction required ${absSet.size} found")
    else LoggedSuccess(log, absSet.head)
  }

 private [rules] def redirect
  (g : DependencyGraph,
   oldUse : Uses,
   newUsed : Abstraction) : LoggedTry[(DependencyGraph, List[Uses])] =
    try LoggedSuccess {
      (newUsed, oldUse.accessKind) match {
        case (AccessAbstraction(absId, _), None) =>
          (oldUse.changeTarget(g, absId), List(oldUse.copy(target = absId)))
        case (ReadWriteAbstraction(Some(rid), _), Some(Read)) =>
          (oldUse.changeTarget(g, rid), List(oldUse.copy(target = rid)))
        case (ReadWriteAbstraction(_, Some(wid)), Some(Write)) =>
          (oldUse.changeTarget(g, wid), List(oldUse.copy(target = wid)))
        case (ReadWriteAbstraction(Some(rid), Some(wid)), Some(RW)) =>
            g.changeTarget(oldUse, rid, wid)
        case _ => throw new PuckError(s"error while redirecting ${(g,oldUse).shows} toward ${(g, newUsed).shows}")
      }
    } catch {
      case e : PuckError => LoggedError(e.getMessage)
    }


  def redirectUsesAndPropagate
    (graph : DependencyGraph,
     oldUse : Uses,
     newUsed : Abstraction
      ): LoggedTG = {

    val g = graph.comment(s"Redirection.redirectUsesAndPropagate(g, ${(graph,oldUse).shows}, ${(graph, newUsed).shows})")

    val oldKindType = g.kindType(oldUse.used)
    val newKindType = newUsed.kindType(g)

    val ltg : LoggedTG = (oldKindType, newKindType) match {
      case (InstanceValueDecl, InstanceValueDecl)
      | (TypeDecl, TypeDecl) =>
        redirectInstanceUsesAndPropagate(g, oldUse, newUsed)

      case (TypeConstructor, InstanceValueDecl) =>
        redirectTypeConstructorToInstanceValueDecl(g, oldUse, newUsed)()

      case (StaticValueDecl, StaticValueDecl)
        | (TypeConstructor, StaticValueDecl) =>
         redirect(g, oldUse, newUsed).map(_._1)

      case (kt1, kt2) =>
        LoggedError(s"redirection of type $kt1 to $kt2 unhandled")
    }

    val log = s"redirect uses and propagate from $oldKindType to $newKindType\n"
    log <++: ltg
  }

  def redirectTypeConstructorToInstanceValueDecl
  (g : DependencyGraph,
   oldUse : Uses,
   newUsed : Abstraction )
  ( createVarStrategy: CreateVarStrategy = CreateTypeMember(g.nodeKindKnowledge.defaultKindForNewReceiver)
    ) : LoggedTG = {
    newUsed.nodes match {
      case List(absNode) =>

        val g1 = oldUse.changeTarget(g, absNode)

        import g.nodeKindKnowledge.intro
        import DGEdge.toPair

        val typeOfNewReveiver = g.container(absNode).get
        val userOfCtor = oldUse.user

        createVarStrategy match {
          case CreateParameter =>
            val decl = g1.getConcreteNode(g1.container_!(userOfCtor))

            intro.parameter(g1, typeOfNewReveiver, decl.id) map {
              case (pNode, g2) =>
                g2.addBinding((pNode.id, typeOfNewReveiver), (userOfCtor, absNode))
            }

          case CreateTypeMember(kind) =>
            intro.typeMember(g1,
              typeOfNewReveiver,
              g.hostTypeDecl(userOfCtor),
              kind) map {
              case (newTypeUse, g2) =>
                intro.addUsesAndSelfDependency(
                  g2.addBinding(newTypeUse, (userOfCtor, absNode)),
                  userOfCtor, newTypeUse.user)
            }
        }

      case _ => LoggedError("constructor should have one abs node")
    }
  }


  def propagateTypeConstraints
    (g : DependencyGraph,
     oldTypeUse : Uses,
     newTypeToUse : NodeId) : LoggedTG = {
//    import puck.util.Debug._
//    println("propagateTypeConstraints")
//    (g, g.edges).println
    val oldTypeUsed = oldTypeUse.used

    val tucs = g.typeConstraints(oldTypeUse)

    val (tucsThatNeedPropagation, tucsThatNeedUpdate) =
      if (g.isa_*(oldTypeUsed, newTypeToUse))
        tucs.partition{
          case Sup(_) | Eq(_) => true
          case Sub(_) => false
        }
      else if (g.isa_*(newTypeToUse, oldTypeUsed))
        tucs.partition{
          case Sub(_) | Eq(_) => true
          case Sup(_) => false
        }
      else
        tucs.partition{
          case Eq(_) => true
          case Sub(_) | Sup(_) => false
        }

    g.abstractions(oldTypeUsed).find (abs => abs.nodes contains newTypeToUse) match {
      case None => LoggedError("no satisfying abstraction found")
      case Some(abs @ AccessAbstraction(absId, _)) =>

        def update(g : DependencyGraph, tuc : TypeUseConstraint) : DependencyGraph =
          g.removeTypeUsesConstraint(oldTypeUse, tuc)
            .addTypeUsesConstraint((oldTypeUse.user, absId), tuc.copyWithNewTypeUsed(absId))

        val g1 = tucsThatNeedUpdate.foldLeft(g)(update)

        tucsThatNeedPropagation.foldLoggedEither(g1) {
          case (g0, tuc) =>
            val (s,t) = tuc.constrainedUse
            val g1 = update(g0, tuc)
            redirectInstanceUsesAndPropagate(g1, Uses(s,t), abs)
        }
    }
  }


  def redirectTypeMemberUses
  ( g : DependencyGraph,
    oldTypeUses : Uses,
    brSet : Set[(Uses, Uses)], // \forall p in brSet, p._1 == oldTypeUses
    newTypeToUse : NodeId
  ): LoggedTG = {
    val newTypeUse = oldTypeUses.copy(target = newTypeToUse)
    brSet.foldLoggedEither(g) {
      case (g00, (_, tmu)) =>
        for {
          abs <- tmAbstraction(g, newTypeToUse, tmu.used)
          gNewTmus <- redirect(g00, tmu, abs)
          (g01, nTmus) = gNewTmus
        } yield {
          nTmus.foldLeft(g01.removeBinding(oldTypeUses, tmu)) {
            _.changeTypeUseOfTypeMemberUse(oldTypeUses, newTypeUse, _)
          }
        }
    }
  }

  def redirectInstanceUsesAndPropagate
    (g : DependencyGraph,
     oldUse : Uses,
     newUsed : Abstraction
      ) : LoggedTG = {
    val log = s"redirectInstanceUsesAndPropagate(g, oldUse = ${(g, oldUse).shows},  " +
              s"newUsed = ${(g, newUsed).shows})\n"

    val newTypeToUse = newUsed.kindType(g) match {
      case TypeDecl =>
        val AccessAbstraction(nid, _) = newUsed
        nid
      case _ => newUsed.containerIn(g).get
    }

    val typeMemberTRset = cl(g, oldUse)

    val ltg : LoggedTG =
      if(typeMemberTRset.nonEmpty) {
        val groupedByTypeUse = typeMemberTRset.groupBy(_._1)

        groupedByTypeUse.toList.foldLoggedEither(g comment log) {
          case (g0, (tu, brSet)) =>
            val g1 = tu.changeTarget(g0, newTypeToUse)
            redirectTypeMemberUses(g1, tu, brSet, newTypeToUse)
        } flatMap {
          g1 =>
            groupedByTypeUse.keys.toList.foldLoggedEither(g1) {
              propagateTypeConstraints(_,_, newTypeToUse)
            }
        }

      }
      else //assert(newUsed.kindType(g) == TypeDecl)
          redirect(g, oldUse, newUsed) flatMap {
            case (g0,_) =>
              propagateTypeConstraints(g0, oldUse, newTypeToUse)
          }

    log <++: ltg
  }


}
