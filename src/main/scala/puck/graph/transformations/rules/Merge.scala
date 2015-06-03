package puck.graph
package transformations
package rules

import puck.PuckError
import puck.graph.DependencyGraph._
import puck.graph.constraints.NotAnAbstraction
import puck.util.Collections._
import scalaz._, Scalaz._

trait MergingCandidatesFinder {

  def mergeMatcherInstances : MergeMatcherInstances

  implicit def mergeMatcher(n : ConcreteNode): MergeMatcher =
    mergeMatcherInstances.semanticMergeMatcher(n)

  def find(g : DependencyGraph, nid : ConcreteNode) : Option[ConcreteNode] = None

  def findIn(g : DependencyGraph, methodId : NodeId,  interfaceId : NodeId): Option[NodeId] =
    findIn(g, g.getConcreteNode(methodId), g.getConcreteNode(interfaceId))

  def findIn(g : DependencyGraph, method : ConcreteNode, interface : ConcreteNode) : Option[NodeId]

}

class Merge
( mergingCandidatesFinder: MergingCandidatesFinder){

  def mergeTypeUsesDependencies
  ( g0 : DependencyGraph,
    consumedId : NodeId,
    consumerId : NodeId
    ) : DependencyGraph = {
    val g = g0.comment(" Merge TypeUses Dependencies")
    val g1 = g.kindType(consumedId) match {
      case TypeMember =>
        g.typeMemberUses2typeUses.foldLeft(g) {
          case (g0, (tmUses, typeUses)) if tmUses.used == consumedId =>
            typeUses.foldLeft(g0) { case (g00, tUse) =>
              g00.addUsesDependency(tUse, tmUses.kind(tmUses.user, consumerId))
                .removeUsesDependency(tUse, tmUses)
            }
          case (g0, _) => g0
        }

      case TypeDecl =>
        g.typeUses2typeMemberUses.foldLeft(g) {
          case (g0, (tUses, typeMemberUses)) if tUses.used == consumedId =>
            typeMemberUses.foldLeft(g0) { case (g00, tmUse) =>
              g00.addUsesDependency(tUses.kind(tUses.user, consumerId), tmUse)
                .removeUsesDependency(tUses, tmUse)
            }
          case (g0, _) => g0
        }
      case _ => ???
    }

    val g2 = g.typeMemberUses2typeUses.foldLeft(g1) {
      case (g0, (tmUses, typeUses)) if tmUses.user == consumedId =>
        typeUses.foldLeft(g0) { case (g00, tUse) =>
          g00.addUsesDependency(tUse, tmUses.kind(consumerId, tmUses.used))
            .removeUsesDependency(tUse, tmUses)
        }
      case (g0, _) => g0
    }

    g.typeUses2typeMemberUses.foldLeft(g2) {
      case (g0, (tUses, typeMemberUses)) if tUses.user == consumedId =>
        typeMemberUses.foldLeft(g0) { case (g00, tmUse) =>
          g00.addUsesDependency(tUses.kind(consumerId, tUses.used), tmUse)
            .removeUsesDependency(tUses, tmUse)
        }
      case (g0, _) => g0
    }

  }


  def mergeChildren
  ( g : DependencyGraph,
    consumedId : NodeId,
    consumerId : NodeId
    ) : LoggedTG =
    foldLoggedOr(g.content(consumedId), g.comment("Merge Children")) {
      (g0, consumedChildId) =>
          mergingCandidatesFinder.findIn(g0, consumedChildId, consumerId) match {
            case Some(consumerChildId) => mergeInto(g0, consumedChildId, consumerChildId)
            case None =>
              g0.kindType(consumedChildId) match {
                case TypeConstructor =>
                  //TODO search for one with compatible arguments
                  g0.content(consumerId).find(g.kindType(_) == TypeConstructor) match {
                    case None => LoggedError(new PuckError("cannot find new type constructor"))
                    case Some(newTypeConstructor) =>

                      foldLoggedOr(g0 usersOf consumedChildId, g0){
                        (g00, userId) =>
                          Redirection.redirectUsesAndPropagate(g00,
                            DGEdge.UsesK(userId, consumedChildId),
                            newTypeConstructor, NotAnAbstraction)
                      }.map(_.removeConcreteNode(consumedChildId))
                  }
                case _ =>
                  LoggedSuccess(g0.changeSource(DGEdge.ContainsK(consumedId, consumedChildId), consumerId)
                    .changeType(consumedChildId, g0.getConcreteNode(consumedChildId).styp, consumedId, consumerId))
              }
          }
      }

  def mergeInto
  ( g : DependencyGraph,
    consumedId : NodeId,
    consumerId : NodeId
    ) : LoggedTG = {
    val log = s"Merging ${g.getNode(consumedId)} into ${g.getNode(consumerId)}"

    val lg = g logComment log

    for {
      g1 <- foldLoggedOr[Set, NodeId, PuckError, DependencyGraph](g.usersOf(consumedId), lg){
        (g0, userId) =>
          g0.set(s"\nredirecting ($userId, $consumedId) toward $consumerId").map{
            _.changeTarget(DGEdge.UsesK(userId, consumedId), consumerId)
              .changeType(userId, g.getConcreteNode(userId).styp, consumedId, consumerId)
          }.toLoggedOr
      }

      g2 <- foldLoggedOr(g1.usedBy(consumedId), g1){
        (g0, usedId) =>
        LoggedSuccess(g0.changeSource(DGEdge.UsesK(consumedId, usedId), consumerId))
      }

      g3 <- foldLoggedOr( g2.directSuperTypes(consumedId), g2){
        (g0, stId) =>
          LoggedSuccess {
            if (stId != consumerId) g0.changeSource(DGEdge.IsaK(consumedId, stId), consumerId)
            else g0.removeIsa(consumedId, stId)
          }
      }

      g4 <- foldLoggedOr(g3.directSubTypes(consumedId), g3) {
        (g0, stId) =>
          LoggedSuccess {
            if (stId != consumerId) g0.changeTarget(DGEdge.IsaK(stId, consumedId), consumerId)
            else g0.removeIsa(stId, consumedId)
          }
      }

      g5 = mergeTypeUsesDependencies(g4, consumedId, consumerId)

      g6 <- foldLoggedOr(g5.abstractions(consumedId), g5){
        (g0, abs) =>
          LoggedSuccess {
            g0.removeAbstraction(consumedId, abs)
              .addAbstraction(consumerId, abs)
          }
      }

      absMap : AbstractionMap = g6.abstractionsMap - consumedId
      newAbsMap : AbstractionMap  = absMap.mapValues {
        case (id , absp) if id == consumedId =>
          (consumerId, absp)
        case v => v
      }

      g7 <- mergeChildren(g6.newGraph(abstractionsMap = newAbsMap), consumedId, consumerId)

    } yield {
        g7.removeContains(g7.container(consumedId).get, consumedId)
            .removeConcreteNode(consumedId)
    }
  }

  def removeConcreteNode
  ( g : DependencyGraph,
    n : ConcreteNode
    ) : LoggedTG = {
    val graph = g.comment(s"Remove node $n")
    for {
      g1 <- foldLoggedOr(graph.content(n.id).map(graph.getConcreteNode), graph)(removeConcreteNode)
      // g1 <- graph.content(n.id).map(graph.getConcreteNode).foldLeftM(graph)(removeConcreteNode)
      g2 <-
      if (g1.usersOf(n.id).nonEmpty)
        LoggedError(new PuckError("Cannot remove a used node"))
      else {
        val g00 = g1.removeContains(g1.container(n.id).get, n.id)
        val g01 = graph.directSuperTypes(n.id).foldLeft(g00) {
          (g, supId) => g.removeIsa(n.id, supId)
        }
        val g02 = graph.usedBy(n.id).foldLeft(g01) {
          (g, usedId) => g.removeUses(n.id, usedId)
        }
        LoggedSuccess(g02.removeConcreteNode(n.id))
      }

    } yield g2
  }
}
