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

import puck.util.LoggedEither._
import puck.graph._
import puck.graph.NodeIdPOps

import scalaz.std.list._
import scalaz.std.set._
import ShowDG._
import puck.PuckError
sealed trait CreateVarStrategy {
  def apply ( g : DependencyGraph, oldTu : NodeIdP, newTu : NodeId, tmUses : Set[NodeIdP] ) : LoggedTG
}
case object CreateParameter extends CreateVarStrategy {
  def apply ( g : DependencyGraph, oldTu : NodeIdP, newTu : NodeId, tmUses : Set[NodeIdP] ) : LoggedTG =
    Move.createParam(g,oldTu, newTu, tmUses)

}
case class CreateTypeMember(kind : NodeKind) extends CreateVarStrategy {
  def apply ( g : DependencyGraph, oldTu : NodeIdP, newTu : NodeId, tmUses : Set[NodeIdP] ) : LoggedTG =
    Move.createTypeMember(g, oldTu, newTu, tmUses, kind)
}

object Move {

  def staticDecl
  ( g : DependencyGraph,
    movedId : NodeId,
    newContainer : NodeId
  ) : LoggedTG =
    g.container(movedId) match {
      case None =>
        LoggedError(s"${(g, movedId).shows} has no container !!!")
      case Some(oldContainer) =>
        val log = s"moving static decl ${(g, movedId).shows} " +
          s"from ${(g, oldContainer).shows} " +
          s"to ${(g, newContainer).shows}"

        (g.comment(s"Move.staticDecl(g, ${(g, movedId).shows}, ${(g, newContainer).shows})").
          changeSource(Contains(oldContainer, movedId), newContainer) logComment log).toLoggedEither
    }

  def usesBetween(g : DependencyGraph, sources :  Set[NodeId], targets : Set[NodeId]): Set[NodeIdP] =
    for{
      s <- sources
      t <- targets
      if g.uses(s,t)
    } yield (s,t)

  def usesViaThis(g : DependencyGraph)(tmu : NodeIdP) : Boolean =
    (g typeUsesOf tmu) exists (_.selfUse)


  def usesBetweenViaThis(g : DependencyGraph, sources :  Set[NodeId], targets : Set[NodeId]): Set[NodeIdP] =
    usesBetween(g, sources, targets) filter usesViaThis(g)


  private abstract class MoveTypeMember
  ( g0 : DependencyGraph,
    typeMembersMovedId : List[NodeId],
    oldContainer : NodeId,
    newContainer : NodeId ){

    val movedDecls = typeMembersMovedId.toSet
    val movedDefs : Set[NodeId] = movedDecls flatMap g0.definitionOf

    val siblings = g0.content(oldContainer) -- movedDecls

    val usesOfSiblingViaThis: Set[NodeIdP] =
      usesBetweenViaThis(g0, movedDefs, siblings)

    val oldSelfUse = Uses(oldContainer, oldContainer)
    val newSelfUse = Uses(newContainer, newContainer)

    val g1 = typeMembersMovedId.foldLeft(g0) {
      (g0, movedId) =>
        g0.changeSource(Contains(oldContainer, movedId), newContainer)
    }

    def adjustSelfUsesBR(g : DependencyGraph) = {
      val usesBetweenMovedDefsViaThis = usesBetweenViaThis(g, movedDefs, movedDecls)

      val g1 =
        if(usesBetweenMovedDefsViaThis.nonEmpty && !(newSelfUse existsIn g))
          newSelfUse createIn g
        else g

      val g2 =
        g1.changeTypeUseForTypeMemberUseSet(oldSelfUse, newSelfUse,
          usesBetweenMovedDefsViaThis)

      if(g2.typeMemberUsesOf(oldSelfUse).isEmpty)
        g2.removeEdge(oldSelfUse)
      else g2
    }

    def apply() : LoggedTG
  }

  def pullUp
  (g0 : DependencyGraph,
   typeMembersMovedId : List[NodeId],
   oldContainer : NodeId,
   newContainer : NodeId) : LoggedTG = {

    val worker : MoveTypeMember =
      new MoveTypeMember(g0, typeMembersMovedId, oldContainer, newContainer) {


        def redirectUsesOfSiblingTowardNewSiblings(g : DependencyGraph) : LoggedTG =
          usesOfSiblingViaThis.foldLoggedEither(g) {
            (g0, e) =>
              g0.abstractions(e.used) find (abs => abs.nodes.forall(g0.contains(newContainer, _))) match {
                case Some(abs) =>
                  Redirection.redirect(g0, e, abs).map {
                    case (g00, lu) =>
                      g00.changeTypeUseForTypeMemberUseSet(oldSelfUse, newSelfUse, lu)
                  }
                case None => LoggedError("pullUp abstract sibling not found")
              }
          }


        def adjustSuperUsesBR(g : DependencyGraph) : DependencyGraph = {
          val brWithOldContainerAsTypeUser = for {
            movedDef <- movedDefs
            used <- g0.usedBy(movedDef)
            tuse <- g0.typeUsesOf(movedDef, used)
            if tuse.user == oldContainer
          } yield {
            (tuse, (movedDef, used))
          }

          brWithOldContainerAsTypeUser.foldLeft(g) {
            case (g00, (tuse, tmUse)) =>
              val newSuperUse = Uses(newContainer, tuse.used)

              val g01 =
                if (!(newSuperUse existsIn g00))
                  g00.addEdge(newSuperUse)
                else g00

              val g02 = g01.changeTypeUseOfTypeMemberUse(tuse, newSuperUse, tmUse)

              if (g02.typeMemberUsesOf(tuse).isEmpty)
                g02.removeEdge(Uses(tuse))
              else g02

          }
        }

        val usesOfMovedViaThis: Set[NodeIdP] =
          usesBetweenViaThis(g0, siblings flatMap g0.definitionOf, movedDecls)

        val newTypeUse = Uses(oldContainer, newContainer)


        def apply() : LoggedTG = {

          val g2 =
            if (usesOfMovedViaThis.nonEmpty && !(newTypeUse existsIn g1))
              g1.addUses(oldContainer, newContainer)
            else g1

          val g3 = g2.changeTypeUseForTypeMemberUseSet(oldSelfUse, newTypeUse, usesOfMovedViaThis)

          val g4 = adjustSelfUsesBR(g3)

          val g5 = adjustSuperUsesBR(g4)

          redirectUsesOfSiblingTowardNewSiblings(g5)

        }
      }
    worker()
  }

  def pushDown
  ( g : DependencyGraph,
    typeMembersMovedId : List[NodeId],
    oldContainer : NodeId,
    newContainer : NodeId) : LoggedTG = LoggedError("TODO : Push down not implemented")

  def typeMember
  ( graph : DependencyGraph,
    typeMembersMovedId : List[NodeId],
    newContainer : NodeId,
    createVarStrategy: Option[CreateVarStrategy] = None) : LoggedTG = {

    val typeMembersMovedStr = typeMembersMovedId.map(nid => (graph, nid).shows ).mkString("[",", ", "]")
    val g0 = graph.comment(s"Move.typeMember(g, $typeMembersMovedStr, ${(graph,newContainer).shows}, $createVarStrategy)")
    /** PRECONDITION all typeMembersMoved have same host */
    val oldContainer = g0 container_! typeMembersMovedId.head

    val isPullUp = g0.isa_*(oldContainer, newContainer)
    val isPushDown = g0.isa_*(newContainer, oldContainer)

    if(isPullUp && isPushDown) {
      assert(oldContainer == newContainer)
      LoggedSuccess(graph)
    }
    else if(isPullUp)
      pullUp(g0, typeMembersMovedId, oldContainer, newContainer)
    else if(isPushDown)
      pushDown(g0, typeMembersMovedId, oldContainer, newContainer)
    else
      typeMemberBetweenUnrelatedTypeDecl(g0, typeMembersMovedId, oldContainer, newContainer, createVarStrategy)

  }



  def typeMemberBetweenUnrelatedTypeDecl
  ( g0 : DependencyGraph,
    typeMembersMovedId : List[NodeId],
    oldContainer : NodeId,
    newContainer : NodeId,
    createVarStrategy: Option[CreateVarStrategy] = None) : LoggedTG = {

    val worker : MoveTypeMember =
      new MoveTypeMember(g0, typeMembersMovedId, oldContainer, newContainer) {

        import puck.util.LoggedEither, LoggedEither._
        def useArgAsReceiver
        ( g: DependencyGraph,
          oldContainer : NodeId,
          newContainer : NodeId,
          nodeSet: Set[NodeId]
        ) : LoggedTG = {
          val newSelfUse = Uses(newContainer, newContainer)
          nodeSet.foldLoggedEither(g){
            (g0, used) =>
              val params = g0 parametersOf used
              params find ( g0.typ(_) uses newContainer ) match {
                case None =>
                  LoggedError(s"An arg typed ${NamedType(newContainer)} was expected")
                case Some(pid) =>
                  val (_, g1) = g0.removeNode(pid)
                  val oldTypeUses = (pid, newContainer)
                  LoggedSuccess (
                    g1.changeTypeUseForTypeMemberUseSet(oldTypeUses, newSelfUse,
                      g0.typeMemberUsesOf(oldTypeUses))
                  )
              }
          }
        }

        def useReceiverAsArg
        ( g: DependencyGraph,
          oldContainer : NodeId,
          newContainer : NodeId,
          nodeSet : Set[NodeId],
          siblings : Set[NodeId]
        ) : LoggedTG = {
          val oldSelfUse = (oldContainer, oldContainer)
          nodeSet.foldLoggedEither(g){
            (g0, user) =>
              val decl = g0.container_!(user)
              addParamOfTypeAndSetTypeDependency(g0, decl,
                oldContainer, oldSelfUse,
                usesBetween(g0, Set(user), siblings))
          }
        }

        def createNewReceiver
        ( g: DependencyGraph,
          newContainer : NodeId,
          usesThatRequireNewReceiver : Set[NodeIdP],
          createVarStrategy: CreateVarStrategy) : LoggedTG = {

          def singleTypeUse(tmUse : NodeIdP) : NodeIdP = {
            val tus =  g.typeUsesOf(tmUse)
            if(tus.size > 1) error("require type member uses with only one type use")
            else tus.head
          }

          try {
            val typeUses = usesThatRequireNewReceiver.groupBy(singleTypeUse).toList

            typeUses.foldLoggedEither(g) {
              case (g0, (tu, tmUses)) =>
                createVarStrategy(g0, tu, newContainer, tmUses)
            }
          }catch {
            case e : PuckError => LoggedError(e)
          }
        }

        val movedDefUsingSiblingViaThis: Set[NodeId] =
          usesOfSiblingViaThis map (_.user)

        val movedDeclWithArgUsableAsReceiver: Set[NodeId] =
          movedDecls.filter {
            nid =>
              g0.parametersOf(nid) exists (g0.typ(_) uses newContainer)
          }

        val movedDeclWithoutArgUsableAsReceiver =
          movedDecls -- movedDeclWithArgUsableAsReceiver

        val usesThatRequireNewReceiver = for {
          declId <- movedDeclWithoutArgUsableAsReceiver
          user <- g0 usersOf declId
          if !(movedDefs contains user)
        } yield (user, declId)

        def apply() : LoggedTG = {

          for {
            g2 <- useArgAsReceiver(g1, oldContainer, newContainer, movedDeclWithArgUsableAsReceiver)

            g3 <- useReceiverAsArg(g2, oldContainer, newContainer, movedDefUsingSiblingViaThis, siblings)

            g4 <- (usesThatRequireNewReceiver.nonEmpty, createVarStrategy) match {
              case (true, Some(strategy)) =>
                createNewReceiver(g3, newContainer, usesThatRequireNewReceiver, strategy)
              case (true, None) => LoggedError("create var strategy required")
              case _ => LoggedSuccess(g3)
            }

          } yield adjustSelfUsesBR(g4)

        }
      }
    worker()
  }



  def addParamOfTypeAndSetTypeDependency
  (g: DependencyGraph,
   declId : NodeId,
   pType : NodeId,
   oldTypeUse : NodeIdP,
   tmUses : Set[NodeIdP]
  ) : LoggedTG  = {
    import g.nodeKindKnowledge.intro
    intro.parameter(g, pType, declId) map {
      case (pNode, g2) =>
        val newTypeUse = (pNode.id, pType)
        g2.changeTypeUseForTypeMemberUseSet(oldTypeUse, newTypeUse, tmUses)

    }
  }




  def createParam
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

        addParamOfTypeAndSetTypeDependency(g0, decl.id,
          newTypeUsed, oldTypeUse, typeMemberUses)

    }
  }

  def createTypeMember
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
