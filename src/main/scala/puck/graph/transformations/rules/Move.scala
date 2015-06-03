package puck.graph.transformations.rules

import puck.PuckError
import puck.graph.ShowDG._
import puck.graph._

import puck.util.Collections._
import puck.graph.transformations.rules.Redirection.redirectUses

import scalaz._, Scalaz._

class Move(intro : Intro) {

  def typeDecl
  ( g : DependencyGraph,
    movedId : NodeId,
    newContainer : NodeId
    ) : LoggedTG =
    g.container(movedId) match {
      case None =>
        LoggedError(new RedirectionError(s"${showDG[NodeId](g).shows(movedId)} has no container !!!"))
      case Some(oldContainer) =>
        val log = s"moving type decl ${showDG[NodeId](g).shows(movedId)} " +
          s"from ${showDG[NodeId](g).shows(oldContainer)} " +
          s"to ${showDG[NodeId](g).shows(newContainer)}"

        (g.changeSource(DGEdge.ContainsK(oldContainer, movedId), newContainer) logComment log).toLoggedOr
    }

  private def withContainer[T]
  (g : DependencyGraph, nid : NodeId)
  ( f : NodeId => Try[T]
    ) : Try[T] =
    g.container(nid) match {
      case None =>
        import ShowDG._
        -\/(new RedirectionError(s"${showDG[NodeId](g).shows(nid)} has no container !!!"))
      case Some(containerId) => f(containerId)
    }


  def usedBySiblingsViaSelf
  ( g : DependencyGraph, n : NodeId, h : CurrentHost ) : Boolean = {
    val uses = g.usersOf(n) map (user => Uses(user, n))
    uses.exists(isSiblingUsesViaSelf(g, h))
  }

  type CurrentHost = DGNode
  def usedBySiblingsViaSelf
    ( uses : Seq[Uses], g : DependencyGraph, h : CurrentHost ) : Boolean =
    uses.exists(isSiblingUsesViaSelf(g, h))

  type SiblingUsers = Set[Uses]
  type OtherUsers = Set[Uses]
  def partitionSiblingUsesAndOtherUses
  ( uses : Set[Uses], g : DependencyGraph, h : CurrentHost ) :(SiblingUsers, OtherUsers) =
    uses.partition(isSiblingUsesViaSelf(g, h))

  private def isSiblingUsesViaSelf
  ( graph : DependencyGraph, currentHost : CurrentHost) : DGUses => Boolean = {

    val isSibling: DGUses => Boolean =
      u => graph.contains(currentHost.id, u.user) && u.user != u.used

    u =>
      isSibling(u) && {
        val tuses = graph.typeUsesOf(u)
        tuses.exists(_.selfUse)
    }
  }


  def redirectTypeUsesOfMovedTypeMemberUsers
  ( g : DependencyGraph,
    typeMemberUses : Seq[Uses],
    newTypeUsed : NodeId
    ) : DependencyGraph =
      typeMemberUses.foldLeft(g) {
        case (g0, typeMemberUse) =>

          g.typeUsesOf(typeMemberUse).foldLeft(g0) { (g0, typeUse) =>

            val g1 = g0.removeUsesDependency(typeUse, typeMemberUse)
            val keepOldUse = g1.typeMemberUsesOf(typeUse).nonEmpty

            val newTypeUse = DGEdge.UsesK(typeUse.user, newTypeUsed)

            redirectUses(g1, typeUse, newTypeUsed, keepOldUse)
              .addUsesDependency(newTypeUse, typeMemberUse)
          }
      }




  private def logTypeMemberMove
  ( g : DependencyGraph,
    typeMembersMoved : Seq[NodeId],
    oldContainer : NodeId,
    newContainer : NodeId
    ) : LoggedTG = {

    val log = "moving type members  " +  typeMembersMoved.map(showDG[NodeId](g).shows).mkString(", ") +
      s"from ${showDG[NodeId](g).shows(oldContainer)} " +
      s"to ${showDG[NodeId](g).shows(newContainer)}"

    (g logComment log).toLoggedOr
  }


  private def foldTypeUsesOf
  ( typeMemberUse : DGUses,
    g : DependencyGraph )
  ( f : (DependencyGraph, DGEdge) => LoggedTG
    ) : LoggedTG =
    foldLoggedOr(g.typeUsesOf(typeMemberUse), g)(f)

  def typeMember
  ( g0 : DependencyGraph,
    typeMembersMovedId : List[NodeId],
    newContainer : NodeId,
    createVarStrategy: Option[CreateVarStrategy] = None)
  (typeMembersMovedUses : List[Uses] = g0.usesOfUsersOf(typeMembersMovedId))
    : LoggedTG = {
    /** PRECONDITION all typeMembersMoved have same host */


    val oldContainer = g0.getConcreteNode(g0.container(typeMembersMovedId.head).get)

    for {
      g <- logTypeMemberMove(g0, typeMembersMovedId, oldContainer.id, newContainer)

      g2 = typeMembersMovedId.foldLeft(g){
            (g0, movedId) =>
              g0.changeSource(DGEdge.ContainsK(oldContainer.id, movedId), newContainer)
          }

          (siblingsUsesViaSelf, otherUses) =
            typeMembersMovedUses.partition(isSiblingUsesViaSelf(g2, oldContainer))

      g3 <- (siblingsUsesViaSelf.nonEmpty, createVarStrategy) match {
            case (true, Some(strategy)) =>
              strategy.moveTypeMemberUsedBySelf(g2,
                oldContainer.id, newContainer,
                siblingsUsesViaSelf, intro)
            case (false, None) => LoggedSuccess(g2)
            case _ => LoggedError[DependencyGraph](new PuckError("incoherent call to Move.typMember"))
         }
    } yield {
      redirectTypeUsesOfMovedTypeMemberUsers(g3, otherUses, newContainer)
    }
  }

}
