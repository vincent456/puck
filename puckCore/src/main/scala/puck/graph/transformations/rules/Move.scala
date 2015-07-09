package puck.graph.transformations.rules

import puck.{graph, PuckError}
import puck.graph._

import scalaz.-\/
import scalaz.std.list._
import scalaz.std.set._
import ShowDG._
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

        (g.changeSource(DGEdge.ContainsK(oldContainer, movedId), newContainer) logComment log).toLoggedEither
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
    ( uses : List[DGUses], g : DependencyGraph, h : CurrentHost ) : Boolean =
    uses.exists(isSiblingUsesViaSelf(g, h))

  type SiblingUsers = Set[Uses]
  type OtherUsers = Set[Uses]
  def partitionSiblingUsesAndOtherUses
  ( uses : Set[Uses], g : DependencyGraph, h : CurrentHost ) :(SiblingUsers, OtherUsers) =
    uses.partition(isSiblingUsesViaSelf(g, h))

  private def isSiblingUsesViaSelf
  ( graph : DependencyGraph, currentHost : CurrentHost) : DGUses => Boolean = {

    val isSiblingUse: DGUses => Boolean =
      u => graph.contains(currentHost.id, u.user) && u.user != u.used

    u =>
      isSiblingUse(u) && {
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

            val newTypeUse = Uses(typeUse.user, newTypeUsed)

            Redirection.redirectUses(g1, typeUse, newTypeUsed, keepOldUse)
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

    (g logComment log).toLoggedEither
  }

  private def isUsesOfOtherMovedNodeViaSelf
  ( graph : DependencyGraph, movedNodes : List[NodeId]) : DGUses => Boolean = {
    u =>
      movedNodes.contains(u.user) && {
        val tuses = graph.typeUsesOf(u)
        tuses.forall(_.selfUse)
      }
  }

  def usesBetween(g : DependencyGraph, sources :  Set[NodeId], targets : Set[NodeId]): Set[DGUses] =
    for{
      s <- sources
      t <- targets
      if g.uses(s,t)
    } yield {
      g.getUsesEdge(s,t).get
    }

  def usesViaThis(g : DependencyGraph)(tmu : DGUses) : Boolean =
    g.typeUsesOf(tmu).exists(tu => tu.source == tu.target)


  def typeMember
  ( g0 : DependencyGraph,
    typeMembersMovedId : List[NodeId],
    newContainer : NodeId,
    createVarStrategy: Option[CreateVarStrategy] = None) : LoggedTG = {

    /** PRECONDITION all typeMembersMoved have same host */
    val oldContainer = g0.container(typeMembersMovedId.head).get
    val moved = typeMembersMovedId.toSet
    val siblings = g0.content(oldContainer) -- moved

    val movedUsingSiblingViaThis : Set[NodeId] =
      usesBetween(g0, moved, siblings).filter(usesViaThis(g0)).map(_.user)

    val movedWithArgUsableAsReceiver : Set[NodeId] =
      moved.filter{
        nid => g0.getConcreteNode(nid).styp match {
          case None => false
          case Some(t) => t uses newContainer
        }
      }

    val oldSelfUse = Uses(oldContainer, oldContainer)
    val newSelfUse = Uses(newContainer, newContainer)

    val g1 = typeMembersMovedId.foldLeft(g0){
      (g0, movedId) =>
        g0.changeSource(Contains(oldContainer, movedId), newContainer)
    }

    val g2 = usesBetween(g0, moved, moved).filter(usesViaThis(g0)).foldLeft(g1){
      (g,u) => g.removeUsesDependency(oldSelfUse, u)
                .addUsesDependency(newSelfUse, u)
    }

    for {
      g3 <- useArgAsReceiver(g2, oldContainer, newContainer, movedWithArgUsableAsReceiver)
      g4 <- useReceiverAsArg(g3, oldContainer, newContainer, movedUsingSiblingViaThis, siblings)
      g5 <- createNewReceiver(g4, oldContainer, newContainer,
        moved -- movedWithArgUsableAsReceiver, createVarStrategy)
    } yield {
      g5
    }

  }

  import puck.util.LoggedEither, LoggedEither._
  def useArgAsReceiver
  ( g: DependencyGraph,
    oldContainer : NodeId,
    newContainer : NodeId,
    nodeSet: Set[NodeId]
    ) : LoggedTG = {
    val newSelfUse = Uses(newContainer, newContainer)
    nodeSet.foldLoggedEither(g){
      (g0, used) => g0.getConcreteNode(used).styp match {
        case Some(ar : Arrow)=>
          val g1 = g0.setType(used,  Some(ar.removeFirstArgOfType(NamedType(newContainer))))
          val oldTypeUses = Uses(used, newContainer)
          val g2 = g0.typeMemberUsesOf(oldTypeUses).foldLeft(g1){
            (g00, tmu) =>
              g00.removeUsesDependency(oldTypeUses, tmu)
                  .addUsesDependency(newSelfUse, tmu)
          }
          LoggedSuccess(g2)
        case _ => LoggedError(new graph.Error("Arrow type was expected"))
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
    val oldSelfUse = Uses(oldContainer, oldContainer)
    nodeSet.foldLoggedEither(g){
      (g0, used) => g0.getConcreteNode(used).styp match {
        case Some(ar : Arrow)=>
          val newTypeUses = Uses(used, oldContainer)
          val g1 = g0.setType(used,  Some(ar.prependParameter(NamedType(oldContainer))))
                      .addEdge(newTypeUses)

          val g2 = usesBetween(g0, Set(used), siblings).foldLeft(g1){
            (g00, tmu) =>
              g00.removeUsesDependency(oldSelfUse, tmu)
                .addUsesDependency(newTypeUses, tmu)
          }
          LoggedSuccess(g2)
        case _ => LoggedError(new graph.Error("Arrow type was expected"))
      }
    }
  }

  def createNewReceiver
  ( g: DependencyGraph,
    oldContainer : NodeId,
    newContainer : NodeId,
    nodeSet : Set[NodeId],
    createVarStrategy: Option[CreateVarStrategy] = None
    ) : LoggedTG = {

    val typeUses : Set[DGUses] =
      g.usesFromUsedList(nodeSet.toList)
        .filterNot(u => nodeSet.contains(u.user))
        .flatMap(g.typeUsesOf).toSet

    createVarStrategy match {
      case Some(strategy) =>

        typeUses.foldLoggedEither(g) {
          (g0, tu) =>
            val tmUses = g.typeMemberUsesOf(tu)
            val tmContainer = if (tu.selfUse) tu.user
            else g.container(tu.user).get
            strategy match {
              case CreateParameter =>
                createParam(g0, tu, newContainer, tmUses)
              case CreateTypeMember(k) =>

                createTypeMember(g0, tu, newContainer, tmContainer, tmUses, k)
            }
        }
      case None if typeUses.nonEmpty =>
        LoggedError(new PuckError("create var strategy required"))
      case None => LoggedSuccess(g)
    }
  }


  def createParam
  ( g : DependencyGraph,
    oldTypeUse : DGUses,
    newTypeUsed : NodeId,
    tmUses : Set[DGUses]
    ): LoggedTG ={

    val usesByUser = tmUses.groupBy(_.user)
    //introduce one parameter by user even with several uses
    //these were all previously this use so it makes sens to keep one reference
    usesByUser.toList.foldLoggedEither(g) {
      case (g0, (userId, typeMemberUses)) =>
        val user = g0.getConcreteNode(userId)
        val newTypeUse = Uses(userId, newTypeUsed)
        (g0.kindType(user), user.styp) match {
          case (TypeMember, Some(NamedType(_))) => ???

          case (TypeMember, Some(ar : Arrow )) =>
            val log = s"${showDG[NodeId](g0).shows(userId)}, user of moved method" +
              s" will now use ${showDG[NodeId](g0).shows(newTypeUsed)}"

            val g1 = g0.setType(userId, Some(ar.prependParameter(NamedType(newTypeUsed))))
              .addEdge(newTypeUse)

            val g2 = typeMemberUses.foldLeft(g1) {
              (g00, typeMemberUse) =>
                g00.removeUsesDependency(oldTypeUse, typeMemberUse)
                  .addUsesDependency(newTypeUse, typeMemberUse)
            }

            LoggedSuccess(g2, log)

          case (_, _) => LoggedError(new PuckError(s"a type member was expected"))
        }
    }

  }

  def createTypeMember
  ( g : DependencyGraph,
    oldTypeUse : DGUses,
    newTypeUsed : NodeId,
    tmContainer : NodeId,
    tmUses : Set[DGUses],
    kind : NodeKind
    ): LoggedTG = {
    val delegateName = s"${g.getConcreteNode(newTypeUsed).name.toLowerCase}_delegate"

    val (delegate, g1) = intro.accessToType(g, delegateName, kind, newTypeUsed)

    val newTypeUse = Uses(delegate.id, newTypeUsed)
    val g2 = g1.addContains(tmContainer, delegate.id)
      .addEdge(newTypeUse) //type field

    tmUses.foldLoggedEither(g2) {
      case (g0, typeMemberUse) =>

        LoggedSuccess(g0.removeUsesDependency(oldTypeUse, typeMemberUse)
          .addUsesDependency(newTypeUse, typeMemberUse)
          .addUses(typeMemberUse.user, delegate.id)) // replace this.m by delegate.m
    }

  }
    /*
      def typeMember
      ( g0 : DependencyGraph,
        typeMembersMovedId : List[NodeId],
        newContainer : NodeId,
        createVarStrategy: Option[CreateVarStrategy] = None)
      (typeMembersMovedUses : List[Uses] = g0.usesFromUsedList(typeMembersMovedId))
        : LoggedTG = {
        /** PRECONDITION all typeMembersMoved have same host */
        val oldContainer = g0.getConcreteNode(g0.container(typeMembersMovedId.head).get)
        val
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
    */

}
