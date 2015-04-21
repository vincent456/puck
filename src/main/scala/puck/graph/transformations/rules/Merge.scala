package puck.graph
package transformations
package rules

import puck.PuckError
import puck.graph.DependencyGraph._
import puck.graph.constraints.NotAnAbstraction
import puck.util.Collections.traverse
import scalaz.{-\/, \/-}

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
  ( g : DependencyGraph,
    consumedId : NodeId,
    consumerId : NodeId) : DependencyGraph = {

    val g1 = g.kindType(consumedId) match {
      case TypeMember =>
        g.typeMemberUses2typeUsesMap.toSeq.foldLeft(g) {
          case (g0, (( typeMemberUserId, `consumedId`), typeUses)) =>
            typeUses.foldLeft(g0) { case (g00, tUse) =>
              g00.addUsesDependency(tUse, (typeMemberUserId, consumerId))
                .removeUsesDependency(tUse, (typeMemberUserId, consumedId))
            }
          case (g0, _) => g0
        }

      case TypeDecl =>
        g.typeUses2typeMemberUsesMap.toSeq.foldLeft(g) {
          case (g0, ((typeUser, `consumedId`), typeMemberUses)) =>
            typeMemberUses.foldLeft(g0) { case (g00, tmUse) =>
              g00.addUsesDependency((typeUser, consumerId), tmUse)
                .removeUsesDependency((typeUser, consumedId), tmUse)
            }
          case (g0, _) => g0
        }
      case _ => ???
    }

    val g2 = g.typeMemberUses2typeUsesMap.toSeq.foldLeft(g1) {
      case (g0, (( `consumedId`, typeMemberUsedId), typeUses)) =>
        typeUses.foldLeft(g0) { case (g00, tUse) =>
          g00.addUsesDependency(tUse, (consumerId, typeMemberUsedId))
            .removeUsesDependency(tUse, (consumedId, typeMemberUsedId))
        }
      case (g0, _) => g0
    }

    g.typeUses2typeMemberUsesMap.toSeq.foldLeft(g2) {
      case (g0, (( `consumedId`, typeUsed), typeMemberUses)) =>
        typeMemberUses.foldLeft(g0) { case (g00, tmUse) =>
          g00.addUsesDependency((consumerId, typeUsed), tmUse)
            .removeUsesDependency((consumedId, typeUsed), tmUse)
        }
      case (g0, _) => g0
    }

  }


  def mergeChildren
  ( g : DependencyGraph,
    consumedId : NodeId,
    consumerId : NodeId) : Try[DependencyGraph] =
    traverse(g.content(consumedId), g) {
      (g0, consumedChildId) =>
          mergingCandidatesFinder.findIn(g0, consumedChildId, consumerId) match {
            case Some(consumerChildId) => mergeInto(g0, consumedChildId, consumerChildId)
            case None =>
              g0.kindType(consumedChildId) match {
                case TypeConstructor =>
                  //TODO search for one with compatible arguments
                  g0.content(consumerId).find(g.kindType(_) == TypeConstructor) match {
                    case None => -\/(new PuckError("cannot find new type constructor"))
                    case Some(newTypeConstructor) =>

                      traverse(g0 usersOf consumedChildId, g0){
                        (g00, userId) =>
                          Redirection.redirectUsesAndPropagate(g00,
                            DGEdge.uses(userId, consumedChildId),
                            newTypeConstructor, NotAnAbstraction)
                      }.map(_.removeConcreteNode(consumedChildId))
                  }
                case _ =>
                  \/-(g0.changeSource(DGEdge.contains(consumedId, consumedChildId), consumerId)
                    .changeType(consumedChildId, g0.getConcreteNode(consumedChildId).styp, consumedId, consumerId))
              }
          }
      }

  def mergeInto
  ( g : DependencyGraph,
    consumedId : NodeId,
    consumerId : NodeId
    ) : Try[DependencyGraph] = {
    g.logger.writeln(s"merging ${g.getNode(consumedId)} into ${g.getNode(consumerId)}" )

    val g1 = g.usersOf(consumedId).foldLeft(g) {
      (g0, userId) =>
        g.logger.writeln(s"redirecting ($userId, $consumedId) toward $consumerId")
        g0.changeTarget(DGEdge.uses(userId, consumedId), consumerId)
          .changeType(userId, g.getConcreteNode(userId).styp, consumedId, consumerId)
    }

    val g2 = g.usedBy(consumedId).foldLeft(g1) {
      (g0, usedId) =>
          g0.changeSource(DGEdge.uses(consumedId, usedId), consumerId)

    }

    val g3 = g.directSuperTypes(consumedId).foldLeft(g2) {
      (g0, stId) =>
        if(stId != consumerId) g0.changeSource(DGEdge.isa(consumedId, stId), consumerId)
        else g0.removeIsa(consumedId, stId)
    }

    val g4 = g.directSubTypes(consumedId).foldLeft(g3) {
      (g0, stId) =>
        if(stId != consumerId) g0.changeTarget(DGEdge.isa(stId, consumedId), consumerId)
        else g0.removeIsa(stId, consumedId)
    }

    val g5 = mergeTypeUsesDependencies(g4, consumedId, consumerId)

    val g6 : DependencyGraph = g5.abstractions(consumedId).foldLeft(g5){
      (g0, abs) =>
        g0.removeAbstraction(consumedId, abs)
          .addAbstraction(consumerId, abs)
    }

    val absMap : AbstractionMap = g6.abstractionsMap - consumedId
    val newAbsMap : AbstractionMap  = absMap.mapValues {
      case (id , absp) if id == consumedId =>
        (consumerId, absp)
      case v => v
    }

    val g7 = g6.newGraph(nAbstractionsMap = newAbsMap)

    val g8 = mergeChildren(g7, consumedId, consumerId)

    g8 map { g =>
      g.removeContains(g.container(consumedId).get, consumedId)
        .removeConcreteNode(consumedId)
    }

  }
  import puck.util.Collections.traverse

  def removeConcreteNode
  ( graph : DependencyGraph,
    n : ConcreteNode
    ) : Try[DependencyGraph] =
    for{
      g1 <- traverse(graph.content(n.id).map(graph.getConcreteNode), graph)(removeConcreteNode)
      // g1 <- graph.content(n.id).map(graph.getConcreteNode).foldLeftM(graph)(removeConcreteNode)
      g2 <-
      if(g1.usersOf(n.id).nonEmpty)
        -\/(new PuckError("Cannot remove a used node"))
      else {
        val g00 = g1.removeContains(g1.container(n.id).get, n.id)
        val g01 = graph.directSuperTypes(n.id).foldLeft(g00){
          (g, supId) => g.removeIsa(n.id, supId)
        }
        val g02 = graph.usedBy(n.id).foldLeft(g01){
          (g, usedId) => g.removeUses(n.id, usedId)
        }
        \/-(g02.removeConcreteNode(n.id))
      }

    } yield g2
}
