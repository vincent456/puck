package puck.graph.transformations.rules

import puck.{graph, PuckError}
import puck.graph._

import scalaz.-\/
import scalaz.std.list._
import scalaz.std.set._
import ShowDG._

sealed trait CreateVarStrategy
case object CreateParameter extends CreateVarStrategy
case class CreateTypeMember(kind : NodeKind) extends CreateVarStrategy

object Move {

  def typeDecl
  ( g : DependencyGraph,
    movedId : NodeId,
    newContainer : NodeId
    ) : LoggedTG =
    g.container(movedId) match {
      case None =>
        LoggedError(new RedirectionError(s"${(g, movedId).shows} has no container !!!"))
      case Some(oldContainer) =>
        val log = s"moving type decl ${(g, movedId).shows} " +
          s"from ${(g, oldContainer).shows} " +
          s"to ${(g, newContainer).shows}"

        (g.changeSource(Contains(oldContainer, movedId), newContainer) logComment log).toLoggedEither
    }

  private def withContainer[T]
  (g : DependencyGraph, nid : NodeId)
  ( f : NodeId => Try[T]
    ) : Try[T] =
    g.container(nid) match {
      case None =>
        import ShowDG._
        -\/(new RedirectionError(s"${(g, nid).shows} has no container !!!"))
      case Some(containerId) => f(containerId)
    }


  def usedBySiblingsViaSelf
  ( g : DependencyGraph, n : NodeId, h : TypeDeclNode ) : Boolean = {
    val uses = g.usersOf(n) map (user => Uses(user, n))
    uses.exists(isUsesOfSiblingViaSelf(g, h))
  }

  type TypeDeclNode = DGNode
  def usedBySiblingsViaSelf
    ( uses : List[DGUses], g : DependencyGraph, h : TypeDeclNode ) : Boolean =
    uses.exists(isUsesOfSiblingViaSelf(g, h))

  type SiblingUsers = Set[Uses]
  type OtherUsers = Set[Uses]
  def partitionSiblingUsesAndOtherUses
  ( uses : Set[Uses], g : DependencyGraph, h : TypeDeclNode ) :(SiblingUsers, OtherUsers) =
    uses.partition(isUsesOfSiblingViaSelf(g, h))

  private def isUsesOfSiblingViaSelf
  ( graph : DependencyGraph, containingTypeDecl : TypeDeclNode) : DGUses => Boolean = {

    val isSiblingUse: DGUses => Boolean =
      u => graph.hostTypeDecl(u.user) == containingTypeDecl.id && u.user != u.used

    u =>
      isSiblingUse(u) && {
        val tuses = graph.typeUsesOf(u)
        tuses.exists(_.selfUse)
    }
  }


  private def logTypeMemberMove
  ( g : DependencyGraph,
    typeMembersMoved : Seq[NodeId],
    oldContainer : NodeId,
    newContainer : NodeId
    ) : LoggedTG = {

    val log = "moving type members  " +
      typeMembersMoved.map(n => (g, n).shows).mkString(", ") +
      s"from ${(g, oldContainer).shows} " +
      s"to ${(g, newContainer).shows}"

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

  def usesViaParamOrField(g : DependencyGraph)(tmu : DGUses) : Boolean =
    g.typeUsesOf(tmu).exists(tu => tu.source != tu.target)



  def typeMember
  ( g0 : DependencyGraph,
    typeMembersMovedId : List[NodeId],
    newContainer : NodeId,
    createVarStrategy: Option[CreateVarStrategy] = None) : LoggedTG = {

    /** PRECONDITION all typeMembersMoved have same host */
    val oldContainer = g0.container(typeMembersMovedId.head).get
    val movedDecl = typeMembersMovedId.toSet
    val siblings = g0.content(oldContainer) -- movedDecl

    val movedDef : Set[NodeId] =
      movedDecl map g0.definition filter (_.nonEmpty) map (_.get)

    val movedDefUsingSiblingViaThis : Set[NodeId] =
      usesBetween(g0, movedDef, siblings).filter(usesViaThis(g0)).map(_.user)


    val movedDeclWithArgUsableAsReceiver : Set[NodeId] =
      movedDecl.filter {
        nid =>
          g0.parameters(nid) exists (g0.styp(_).get uses newContainer)
      }

    val oldSelfUse = Uses(oldContainer, oldContainer)
    val newSelfUse = Uses(newContainer, newContainer)

    val g1 = typeMembersMovedId.foldLeft(g0){
      (g0, movedId) =>
        g0.changeSource(Contains(oldContainer, movedId), newContainer)
    }

    val g2 = usesBetween(g0, movedDef, movedDef).filter(usesViaThis(g0)).foldLeft(g1){
          _.changeTypeUseOfTypeMemberUse(oldSelfUse, newSelfUse, _)
    }

   /* println("moving " + movedDecl)
    println("siblings are " + siblings)
    println("moved using sibling via def " +
      (movedDefUsingSiblingViaThis map g0.container_!))
    println("moved with arg usable as receiver" + movedDeclWithArgUsableAsReceiver)
*/
    for {
      g3 <- useArgAsReceiver(g2, oldContainer, newContainer, movedDeclWithArgUsableAsReceiver)
      g4 <- useReceiverAsArg(g3, oldContainer, newContainer, movedDefUsingSiblingViaThis, siblings)
      g5 <- createNewReceiver(g4, newContainer,
        movedDecl -- movedDeclWithArgUsableAsReceiver, siblings,
        createVarStrategy)
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
      (g0, used) =>
        val params = g0 parameters used
        params find {g0.styp(_) contains NamedType(newContainer)} match {
          case None =>
            LoggedError(new PuckError(s"An arg typed ${NamedType(newContainer)} was expected"))
          case Some(pid) =>
            val (_, g1) = g0.removeNode(pid)
            val oldTypeUses = Uses(pid, newContainer)
            val g2 = g0.typeMemberUsesOf(oldTypeUses).foldLeft(g1){
                _.changeTypeUseOfTypeMemberUse(oldTypeUses, newSelfUse, _)
            }
            LoggedSuccess(g2)
        }
    }
  }

  def addParamOfTypeAndSetTypeDependency
  ( g: DependencyGraph,
    declId : NodeId,
    pType : NodeId,
    oldTypeUse : DGUses,
    tmUses : Set[DGUses]
    ) : LoggedTG  = {
    import g.nodeKindKnowledge.intro
    intro.parameter(g, pType, declId) map {
      case (pNode, g2) =>
        val newTypeUse = Uses(pNode.id, pType)

        tmUses.foldLeft(g2) {
          _.changeTypeUseOfTypeMemberUse(oldTypeUse, newTypeUse, _)
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
      (g0, user) =>
        val decl = g0.container_!(user)

        addParamOfTypeAndSetTypeDependency(g0, decl,
            oldContainer, oldSelfUse, usesBetween(g0, Set(user), siblings))
    }
  }

  def createNewReceiver
  ( g: DependencyGraph,
    newContainer : NodeId,
    declNodeSet : Set[NodeId],
    tmUsedToKeep : Set[NodeId] = Set(), //when declNodeSet are moved nodes, not used for "simple" redirection
    createVarStrategy: Option[CreateVarStrategy] = None) : LoggedTG = {


    val defNodeSet = declNodeSet map g.definition filter (_.nonEmpty) map (_.get)

    val typeUses : Set[DGUses] =
      g.usesFromUsedList(declNodeSet.toList)
        .filterNot(u => defNodeSet.contains(u.user))
        .flatMap(g.typeUsesOf).toSet

    createVarStrategy match {
      case Some(strategy) =>

        typeUses.foldLoggedEither(g) {
          (g0, tu) =>
            val tmUses = g.typeMemberUsesOf(tu).filterNot(tmUsedToKeep contains _.used)
            val tmContainer =
              if (tu.selfUse) tu.user
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
    someOldTypeUse : DGUses,
    newTypeUsed : NodeId,
    tmUses : Set[DGUses]
    ): LoggedTG ={

    val usesByUser = tmUses.groupBy(_.user)
    //introduce one parameter by user even with several uses
    //these were all previously this use so it makes sens to keep one reference
    usesByUser.toList.foldLoggedEither(g) {
      case (g0, (impl, typeMemberUses)) =>
        val user = g0.getConcreteNode(impl)

        assert(g0.getConcreteNode(impl).kind.kindType == ValueDef)

        val decl = g0.getConcreteNode(g0.container_!(impl))

        addParamOfTypeAndSetTypeDependency(g0, decl.id,
          newTypeUsed, someOldTypeUse, typeMemberUses)

    }
  }

  def createTypeMember
  ( g : DependencyGraph,
    oldTypeUse : DGUses,
    newTypeUsed : NodeId,
    tmContainer : NodeId,
    tmUses : Set[DGUses],
    kind : NodeKind
    ): LoggedTG ={
    import g.nodeKindKnowledge.intro
    for {
      ug <- intro.typeMember(g, newTypeUsed, tmContainer, kind)
    } yield {
      val (newTypeUse, g2) = ug
      val delegateId = newTypeUse.user

      tmUses.foldLeft(g2) {
        case (g0, typeMemberUse) =>
            g0.changeTypeUseOfTypeMemberUse(oldTypeUse, newTypeUse, typeMemberUse)
              .addUses(typeMemberUse.user, delegateId) // replace this.m by delegate.m
      }
    }




  }



}
