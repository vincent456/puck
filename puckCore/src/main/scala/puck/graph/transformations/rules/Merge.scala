package puck.graph
package transformations
package rules

import puck.PuckError
import puck.graph.DependencyGraph.AbstractionMap
import puck.graph.{TypeConstructor, StaticValueDecl}
import puck.graph.constraints.NotAnAbstraction
import puck.util.LoggedEither._
import ShowDG._
import scalaz.std.list._
import scalaz.std.set._

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
      case InstanceValueDecl
         | StaticValueDecl =>
        g.typeMemberUses2typeUses.foldLeft(g) {
          case (g0, (tmUses, typeUses)) if tmUses.used == consumedId =>
            typeUses.foldLeft(g0) { case (g00, tUse) =>
              g00.changeTypeMemberUseOfTypeUse(tmUses, (tmUses.user, consumerId), tUse)
            }
          case (g0, _) => g0
        }

      case TypeDecl =>
        g.typeUses2typeMemberUses.foldLeft(g) {
          case (g0, (tUses, typeMemberUses)) if tUses.used == consumedId =>
            typeMemberUses.foldLeft(g0) { case (g00, tmUse) =>
              g00.changeTypeUseOfTypeMemberUse(tUses, (tUses.user, consumerId), tmUse)
            }
          case (g0, _) => g0
        }
      case NameSpace // no type dependencies involved when merging namespaces
        | TypeConstructor => g
      case _ => ???
    }

    val g2 = g.typeMemberUses2typeUses.foldLeft(g1) {
      case (g0, (tmUses, typeUses)) if tmUses.user == consumedId =>
        typeUses.foldLeft(g0) { case (g00, tUse) =>
          g00.changeTypeMemberUseOfTypeUse(tmUses, (consumerId, tmUses.used), tUse)
        }
      case (g0, _) => g0
    }

    g.typeUses2typeMemberUses.foldLeft(g2) {
      case (g0, (tUses, typeMemberUses)) if tUses.user == consumedId =>
        typeMemberUses.foldLeft(g0) { case (g00, tmUse) =>
          g00.changeTypeUseOfTypeMemberUse(tUses, (consumerId, tUses.used), tmUse)
        }
      case (g0, _) => g0
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

//                      (g0 usersOf consumedChildId).foldLoggedEither(g0){
//                        (g00, userId) =>
//                          val uses = g00.getUsesEdge(userId, consumedChildId).get
//                          LoggedSuccess(uses.changeTarget(g00, newTypeConstructor))
//                      }.map(_.removeNode(consumedChildId)._2)

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
    val g = graph.comment(s"Merge.mergeInto(g, ${(graph, consumedId).shows}, ${(graph, consumerId).shows})")
    val consumed = g.getConcreteNode(consumedId)
    consumed.kind.kindType match {
      case InstanceValueDecl
      | StaticValueDecl
      | TypeConstructor =>
        mergeInto0(g, consumedId, consumerId){
          (g, consumedId, _) =>
            val content = g.definitionOf(consumedId) match {
              case None => g.parametersOf(consumedId)
              case Some(d) => d :: g.parametersOf(consumedId)
            }
//            val content = g.content(consumedId)
            content.foldLoggedEither(g.comment("Delete consumed def")) {
              (g, cid) => removeConcreteNode(g, g.getConcreteNode(cid))
            }
        }

      case TypeDecl =>
        val g1 = g.typedBy(consumedId).foldLeft(g){
          (g0, t) =>
            g0.getConcreteNode(t).kind.kindType match {
              case TypeConstructor => g0.setType(t, None)
              case _ => g0.changeType(t, consumedId, consumerId)
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
    val log = s"Merging ${g.getNode(consumedId)} into ${g.getNode(consumerId)}"

    val lg = g logComment log

    for {
      g1 <- g.usersOfExcludingTypeUse(consumedId).foldLoggedEither[PuckError, DependencyGraph](lg){
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

      g5 = mergeTypeUsesDependencies(g4, consumedId, consumerId)

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
      (g8 removeNode consumedId)._2.setType(consumedId, None)
    }
  }

  def removeTypeDependenciesInvolving(g : DependencyGraph, n : ConcreteNode) : DependencyGraph = {
    val g1 = n.kind.kindType match {
      case TypeDecl
        | InstanceTypeDecl =>
        g.typeUses2typeMemberUses.foldLeft(g) {
          case (g0, (tUses, typeMemberUses)) if tUses.used == n.id =>
            typeMemberUses.foldLeft(g0) { _.removeUsesDependency(tUses, _)}
          case (g0, _) => g0
        }

      case NameSpace  => g // no type dependencies involved when removing namespaces
      case InstanceValueDecl
           | StaticValueDecl
           | TypeConstructor
           | Parameter
           | ValueDef =>
        g.typeUses2typeMemberUses.foldLeft(g) {
          case (g0, (tUses, typeMemberUses)) if tUses.user == n.id =>
            typeMemberUses.foldLeft(g0) { _.removeUsesDependency(tUses, _)}
          case (g0, _) => g0
        }
      case UnknownKindType => error("removeTypeDependenciesInvolving UnknownKindType")

    }
   /* val g2 =*/ n.kind.kindType match {
      case ValueDef =>
          g1.typeMemberUses2typeUses.foldLeft(g1) {
            case (g0, (tmUses, typeUses)) if tmUses.user == n.id =>
              typeUses.foldLeft(g0) {
                _.removeUsesDependency(_, tmUses)
              }
            case (g0, _) => g0
          }
      case _ => g1
    }
//    println("typeUses2typeMemberUses " + g2.typeUses2typeMemberUses)
//    println("typeMemberUses2typeUses " + g2.typeMemberUses2typeUses)
//    g2


  }

  def removeConcreteNode
  ( g : DependencyGraph,
    n : ConcreteNode
    ) : LoggedTG = {
    val graph = g.comment(s"Remove($n)")
    for {
      g1 <- graph.content(n.id).map(graph.getConcreteNode).
          foldLoggedEither(graph)(removeConcreteNode)
      // g1 <- graph.content(n.id).map(graph.getConcreteNode).foldLeftM(graph)(removeConcreteNode)
      g2 <-
      if (g1.usersOfExcludingTypeUse(n.id).nonEmpty)
        LoggedError("Cannot remove a used node")
      else {
        val g00 =
          n.kind.kindType match {
            case Parameter =>
              g1.removeEdge(ContainsParam(g1.container(n.id).get, n.id))
//            case ValueDef =>
//              g1.removeEdge(ContainsDef(g1.container(n.id).get, n.id))
            case _ => g1.removeEdge(Contains(g1.container(n.id).get, n.id))
          }

        val g01 = graph.directSuperTypes(n.id).foldLeft(g00) {
          (g, supId) => g.removeIsa(n.id, supId)
        }
        val g02 = graph.usedByExcludingTypeUse(n.id).foldLeft(g01) {
          (g, usedId) =>
            //getUsesEdge needed to recover accessKind
            g.removeEdge(g.getUsesEdge(n.id, usedId).get)
        }
        val g03 = removeTypeDependenciesInvolving(g02.setType(n.id, None), n)
        LoggedSuccess(g03.removeConcreteNode(n))
      }

    } yield g2
  }
}
