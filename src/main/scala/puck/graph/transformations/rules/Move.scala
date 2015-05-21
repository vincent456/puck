package puck.graph.transformations.rules

import puck.PuckError
import puck.graph.ShowDG._
import puck.graph._

import puck.util.Collections.traverse
import puck.graph.transformations.rules.Redirection.redirectUses

import scalaz.{\/-, -\/}

class Move(intro : Intro) {

  def typeDecl
  ( g0 : DependencyGraph,
    movedId : NodeId,
    newContainer : NodeId
    ) : Try[DependencyGraph] =
    withContainer(g0, movedId){
      oldContainer =>
        val log = s"moving type decl ${showDG[NodeId](g0).shows(movedId)} " +
          s"from ${showDG[NodeId](g0).shows(oldContainer)} " +
          s"to ${showDG[NodeId](g0).shows(newContainer)}"
        val g = g0.comment(log)
        g.logger.writeln(log)
        \/-(g.changeSource(DGEdge.ContainsK(oldContainer, movedId), newContainer))
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
    ) : DependencyGraph = {

    val log = "moving type members  " +  typeMembersMoved.map(showDG[NodeId](g).shows).mkString(", ") +
      s"from ${showDG[NodeId](g).shows(oldContainer)} " +
      s"to ${showDG[NodeId](g).shows(newContainer)}"
    g.logger.writeln(log)
    g.comment(log)
  }


  private def foldTypeUsesOf
  ( typeMemberUse : DGUses,
    g : DependencyGraph )
  ( f : (DependencyGraph, DGEdge) => Try[DependencyGraph]
    ) : Try[DependencyGraph] =
    traverse(g.typeUsesOf(typeMemberUse), g)(f)

  def typeMember
  ( g0 : DependencyGraph,
    typeMembersMovedId : Seq[NodeId],
    newContainer : NodeId,
    createVarStrategy: Option[CreateVarStrategy] = None)
  (typeMembersMovedUses : Seq[Uses] = g0.usesOfUsersOf(typeMembersMovedId))
    : Try[DependencyGraph] = {
    /** PRECONDITION all typeMembersMoved have same host */


    val oldContainer = g0.getConcreteNode(g0.container(typeMembersMovedId.head).get)

    val g = logTypeMemberMove(g0, typeMembersMovedId, oldContainer.id, newContainer)

    val g2 = typeMembersMovedId.foldLeft(g){
      (g0, movedId) =>
        g0.changeSource(DGEdge.ContainsK(oldContainer.id, movedId), newContainer)
    }

    val (siblingsUsesViaSelf, otherUses) =
      typeMembersMovedUses.partition(isSiblingUsesViaSelf(g2, oldContainer))

    val tg : Try[DependencyGraph] = (siblingsUsesViaSelf.nonEmpty, createVarStrategy) match {
      case (true, Some(strategy)) =>
        strategy.moveTypeMemberUsedBySelf(g2,
          oldContainer.id, newContainer,
          siblingsUsesViaSelf, intro)
      case (false, None) => \/-(g2)
      case _ => -\/(new PuckError("incoherent call to Move.typMember"))
    }
    tg.map(redirectTypeUsesOfMovedTypeMemberUsers(_, otherUses, newContainer))
  }

}
