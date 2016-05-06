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

package puck.graph
package transformations
package rules

import puck.PuckError
import puck.graph.DependencyGraph.AbstractionMap
import puck.graph.constraints.ConstraintsMaps
import puck.util.LoggedEither._
import ShowDG._

import scalaz.std.set._
import scalaz.std.list._

trait MergingCandidatesFinder {

  def mergeMatcherInstances : MergeMatcherInstances

  implicit def mergeMatcher(n : ConcreteNode): MergeMatcher =
    mergeMatcherInstances.semanticMergeMatcher(n)

  def find(g : DependencyGraph, nid : ConcreteNode)(implicit constraints: ConstraintsMaps) : Option[ConcreteNode] = None

  def findIn(g : DependencyGraph, methodId : NodeId,  interfaceId : NodeId): Option[NodeId] =
    findIn(g, g.getConcreteNode(methodId), g.getConcreteNode(interfaceId))

  def findIn(g : DependencyGraph, method : ConcreteNode, interface : ConcreteNode) : Option[NodeId]

}

class Merge
( mergingCandidatesFinder: MergingCandidatesFinder){

  def mergeBindingRelationship
  ( g0 : DependencyGraph,
    consumedId : NodeId,
    consumerId : NodeId
    ) : DependencyGraph = {
    val g = g0.comment("Merge TypeUses Dependencies")
    import ShowDG._
    val log = s"consumed= $consumedId - ${(g, consumedId).shows(desambiguatedFullName)}, " +
      s"consumer= $consumerId - ${(g, consumerId).shows(desambiguatedFullName)}"


    def changeTypeBindings
    ( g : DependencyGraph,
      use : NodeIdP,
      f : (NodeIdP) => DependencyGraph
      ) : DependencyGraph = {
      use match {
        case (`consumedId`, `consumedId`) => f((consumerId, consumerId))
        case (user, `consumedId`) => f((user, consumerId))
        case (`consumedId`, used) => f((consumerId, used))
        case _ => g
      }
    }

    def changeTypeUseForTypeMemberUses
    ( g : DependencyGraph,
      tu2tmus : (NodeIdP, Set[NodeIdP])) : DependencyGraph ={
      val (tUse, tmUses) = tu2tmus
      changeTypeBindings( g, tUse,
        g.changeTypeUseForTypeMemberUseSet(tUse, _, tmUses))
    }


    def changeTypeMemberUseForTypeUses
    ( g : DependencyGraph,
      tmu2tus : (NodeIdP, Set[NodeIdP])) : DependencyGraph = {
      val (tmu, tus) = tmu2tus
      changeTypeBindings(g, tmu,
        g.changeTypeMemberUseOfTypeUseSet(tmu, _, tus))
    }



    g.kindType(consumedId) match {
      case InstanceValueDecl =>
        val g1 = g.typeMemberUses2typeUses.foldLeft(g)(changeTypeMemberUseForTypeUses)
        g1.typeUses2typeMemberUses.foldLeft(g1)(changeTypeUseForTypeMemberUses)

      case TypeDecl
        | Parameter => g.typeUses2typeMemberUses.foldLeft(g)(changeTypeUseForTypeMemberUses)

      case NameSpace // no type dependencies involved when merging namespaces
        | TypeConstructor
        | StaticValueDecl => g

     // case _ => ???
    }
  }

  def mergeTypeConstraints
  ( g0 : DependencyGraph,
    consumedId : NodeId,
    consumerId : NodeId
  ) : DependencyGraph ={
    val it : Iterator[(NodeIdP, TypeUseConstraint)] =
      g0.kindType(consumedId) match {
      case NameSpace => Iterator.empty
      case TypeDecl =>
        g0.typeConstraintsIterator.filter {
          case ((user, used), ct) => user == consumedId || used == consumedId
        }
      case _ =>
        g0.typeConstraintsIterator.filter {
          case ((user, used), ct) => user == consumedId
        }
    }
    val map : NodeId => NodeId = id => if (id == consumedId) consumerId else id
    it.foldLeft(g0){
      case (g, ((user, used), tuc)) =>
        g.removeTypeUsesConstraint((user, used), tuc).
          addTypeUsesConstraint((map(user), map(used)),
            tuc.copyWith(user = map(tuc.constrainedUser), used = map(tuc.constrainedType)))

    }

  }

  def mergeChildrenOfTypeDecl
  ( g : DependencyGraph,
    consumedId : NodeId,
    consumerId : NodeId
    ) : LoggedTG =
    g.content(consumedId).foldLoggedEither(g.comment("Merge Children of TypeDecl")) {
      (g0, consumedChildId) =>
          mergingCandidatesFinder.findIn(g0, consumedChildId, consumerId) match {
            case Some(consumerChildId) =>
              mergeInto(g0, consumedChildId, consumerChildId)
            case None =>
              g0.kindType(consumedChildId) match {
                case TypeConstructor =>
                  //TODO search for one with compatible arguments
                  g0.content(consumerId).find(g.kindType(_) == TypeConstructor) match {
                    case None => LoggedError("cannot find new type constructor")
                    case Some(newTypeConstructor) =>
                      mergeInto(g0, consumedChildId, newTypeConstructor)

                  }
                case _ =>
                  LoggedSuccess(g0.changeSource(Contains(consumedId, consumedChildId), consumerId))
              }
          }
      }

  type MergeIntoFun =(DependencyGraph, NodeId, NodeId) => LoggedTG

  def mergeInto
  ( graph : DependencyGraph,
    consumedId : NodeId,
    consumerId : NodeId
    ) : LoggedTG = {
    val g = graph.comment(s"Merging ${(graph, consumedId).shows} into ${(graph, consumerId).shows}")
    val consumed = g.getConcreteNode(consumedId)
    consumed.kind.kindType match {
      case InstanceValueDecl
      | StaticValueDecl
      | TypeConstructor =>
        mergeInto0(g, consumedId, consumerId){
          (g, consumedId, consumerId) =>
            val ps = g.parametersOf(consumedId).zip(g.parametersOf(consumerId))

            for {
              g0 <- g.definitionOf(consumedId).map {
                cid => Remove.concreteNode(g, g.getConcreteNode(cid))
              } getOrElse LoggedSuccess(g)
              g1 <- ps.foldLoggedEither(g0){
                case (g, (pConsumed, pConsumer)) =>
                  mergeInto0(g, pConsumed, pConsumer)((g,_,_) =>
                    LoggedSuccess(g.removeEdge(ContainsParam(consumedId, pConsumed))))
              }
            } yield g1
        }

      case TypeDecl =>
        val g1 = g.typedBy(consumedId).foldLeft(g){
          (g0, t) =>
            g0.getConcreteNode(t).kind.kindType match {
              case TypeConstructor => g0.removeType(t)
              case _ => g0.changeTarget(Uses(t, consumedId), consumerId)
            }
        }
        mergeInto0(g1, consumedId, consumerId)(mergeChildrenOfTypeDecl)

      case kt =>
        LoggedError(s"$kt consumed kindType unhandled")
    }
  }

    def mergeInto0
  ( g : DependencyGraph,
    consumedId : NodeId,
    consumerId : NodeId)
  ( mergeChildren : MergeIntoFun ) : LoggedTG = {

    for {
      g1 <- g.usersOfExcludingTypeUse(consumedId).foldLoggedEither[PuckError, DependencyGraph](g){
        (g0, userId) =>
          if(userId == consumedId) LoggedSuccess(g0.removeEdge(Uses(userId, userId)))
          else
            LoggedSuccess(s"redirecting ($userId, $consumedId) toward $consumerId\n",
              g0.changeTarget(Uses(userId, consumedId), consumerId))
      }


      g2 <- g1.usedByExcludingTypeUse(consumedId).foldLoggedEither(g1){
        (g0, usedId) =>
          LoggedSuccess(g0.changeSource(Uses(consumedId, usedId), consumerId))
      }

      g3 <- g2.directSuperTypes(consumedId).foldLoggedEither(g2){
        (g0, stId) =>
          LoggedSuccess {
            if (stId != consumerId) g0.changeSource(Isa(consumedId, stId), consumerId)
            else g0.removeIsa(consumedId, stId)
          }
      }

      g4 <- g3.directSubTypes(consumedId).foldLoggedEither(g3) {
        (g0, stId) =>
          LoggedSuccess {
            if (stId != consumerId) g0.changeTarget(Isa(stId, consumedId), consumerId)
            else g0.removeIsa(stId, consumedId)
          }
      }

      g5 = mergeTypeConstraints(
        mergeBindingRelationship(g4, consumedId, consumerId),
        consumedId, consumerId)

      g6 <- g5.abstractions(consumedId).foldLoggedEither(g5){
        (g0, abs) =>
          LoggedSuccess {
            g0.removeAbstraction(consumedId, abs)
              .addAbstraction(consumerId, abs)
          }
      }

      absMap : AbstractionMap = g6.abstractionsMap - consumedId
      newAbsMap : AbstractionMap  = absMap.mapValues {
        case AccessAbstraction(`consumedId`, absp) =>
          AccessAbstraction(consumerId, absp)
        case ReadWriteAbstraction(sRid, sWid) =>
          val condRep : NodeId => NodeId = id =>
            if(id == consumedId) consumerId
            else id
          ReadWriteAbstraction(sRid map condRep, sWid map condRep)
        case v => v
      }

      g7 <- mergeChildren(g6.newGraph(abstractionsMap = newAbsMap), consumedId, consumerId)

    } yield {
      val g8 = g7.container(consumedId) match {
        case Some(cid) => g7.removeContains(cid, consumedId)
        case None => g7
      }
      (g8 removeNode consumedId)._2.removeType(consumedId)
    }
  }





}
