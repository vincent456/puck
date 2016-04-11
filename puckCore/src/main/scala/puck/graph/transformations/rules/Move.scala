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

import scalaz.-\/
import scalaz.std.list._
import scalaz.std.set._
import ShowDG._
sealed trait CreateVarStrategy
case object CreateParameter extends CreateVarStrategy
case class CreateTypeMember(kind : NodeKind) extends CreateVarStrategy

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

  private def withContainer[T]
  (g : DependencyGraph, nid : NodeId)
  ( f : NodeId => Try[T]
    ) : Try[T] =
    g.container(nid) match {
      case None =>
        -\/(new RedirectionError(s"${(g, nid).shows} has no container !!!"))
      case Some(containerId) => f(containerId)
    }


  def usedBySiblingsViaSelf
  ( g : DependencyGraph, n : NodeId, h : TypeDeclNode ) : Boolean = {
    val uses = g.usersOfExcludingTypeUse(n) map (user => Uses(user, n))
    uses.exists(isUsesOfSiblingViaSelf(g, h))
  }

  type TypeDeclNode = DGNode
  def usedBySiblingsViaSelf
    (uses : List[Uses], g : DependencyGraph, h : TypeDeclNode ) : Boolean =
    uses.exists(isUsesOfSiblingViaSelf(g, h))

  type SiblingUsers = Set[Uses]
  type OtherUsers = Set[Uses]
  def partitionSiblingUsesAndOtherUses
  (uses : Set[Uses], g : DependencyGraph, h : TypeDeclNode ) :(SiblingUsers, OtherUsers) =
    uses.partition(isUsesOfSiblingViaSelf(g, h))

  private def isUsesOfSiblingViaSelf
  ( graph : DependencyGraph, containingTypeDecl : TypeDeclNode) : Uses => Boolean = {

    val isSiblingUse: Uses => Boolean =
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
  ( graph : DependencyGraph, movedNodes : List[NodeId]) : Uses => Boolean = {
    u =>
      movedNodes.contains(u.user) && {
        val tuses = graph.typeUsesOf(u)
        tuses.forall(_.selfUse)
      }
  }

  def usesBetween(g : DependencyGraph, sources :  Set[NodeId], targets : Set[NodeId]): Set[Uses] =
    for{
      s <- sources
      t <- targets
      if g.uses(s,t)
    } yield {
      g.getUsesEdge(s,t).get
    }

  def usesViaThis(g : DependencyGraph)(tmu : Uses) : Boolean =
    g.typeUsesOf(tmu).exists(tu => tu.source == tu.target)

  def usesViaParamOrField(g : DependencyGraph)(tmu : Uses) : Boolean =
    g.typeUsesOf(tmu).exists(tu => tu.source != tu.target)


  def pullUp
  ( g : DependencyGraph,
    typeMembersMovedId : List[NodeId],
    oldContainer : NodeId,
    newContainer : NodeId) : LoggedTG = {

    val movedDeclSet = typeMembersMovedId.toSet
    val movedDefSet : Set[NodeId] =
      movedDeclSet map g.definitionOf flatten

    val siblings = g.content(oldContainer) -- movedDeclSet

    val oldSelfUse = Uses(oldContainer, oldContainer)
    val newSelfUse = Uses(newContainer, newContainer)


    val g0 = typeMembersMovedId.foldLeft(g){
      (g0, movedId) =>
        g0.changeSource(Contains(oldContainer, movedId), newContainer)
    }

    val g1 = adjustSelfUsesBR(g0, movedDeclSet, movedDefSet, oldSelfUse, newSelfUse)



    val usesOfMovedViaThis : Set[Uses] =
      usesBetween(g, siblings map (g.definitionOf(_)) flatten, movedDeclSet).filter(usesViaThis(g))

    val newTypeUse = Uses(oldContainer, newContainer)
    val g2 =
      if(usesOfMovedViaThis.nonEmpty && ! (newTypeUse existsIn g1) )
        g1.addUses(oldContainer, newContainer)
      else g1

    val g3 = usesOfMovedViaThis.foldLeft(g2){
      (g00, u) =>
        g00.changeTypeUseOfTypeMemberUse(oldSelfUse, newTypeUse, u)
    }

    val brWithOldContainerAsTypeUser = for {
      movedDef <- movedDefSet
      used <-  g.usedByExcludingTypeUse(movedDef)
      tuse <- g.typeUsesOf(movedDef, used)
      if tuse.user == oldContainer
    } yield {
      (tuse, (movedDef, used))
    }

    val g4 = brWithOldContainerAsTypeUser.foldLeft(g3){
      case (g00, (tuse, tmUse)) =>
        val newSuperUse = Uses(newContainer, tuse.used)

        val g01 =
          if(!(newSuperUse existsIn g00))
            g00.addEdge(newSuperUse)
        else g00

        val g02 = g01.changeTypeUseOfTypeMemberUse(tuse, newSuperUse, tmUse)

        if(g02.typeMemberUsesOf(tuse).isEmpty)
          g02.removeEdge(tuse)
        else g02

    }

    val usesOfSiblingViaThis : Set[Uses] =
      usesBetween(g, movedDefSet, siblings).filter(usesViaThis(g))



    val g5 =
      if(usesOfSiblingViaThis.nonEmpty && !(newSelfUse existsIn g4))
        newSelfUse createIn g4
      else g4

    usesOfSiblingViaThis.foldLoggedEither(g5){
      (g0, e) =>
          g.abstractions(e.target) find (abs => abs.nodes.forall(g.contains(newContainer,_))) match {
            case Some(abs) =>
              Redirection.redirect(g, e, abs).map {
                case (g, lu) =>
                  lu.foldLeft(g)(_.changeTypeUseOfTypeMemberUse(oldSelfUse, newSelfUse, _))
              }
            case None => LoggedError("pullUp abstract sibling not found")
          }
    }


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
    val oldContainer = g0.container(typeMembersMovedId.head).get

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

  private def adjustSelfUsesBR
  (g : DependencyGraph,
   movedDecl : Set[NodeId],
   movedDef : Set[NodeId],
   oldSelfUse : Uses,
   newSelfUse : Uses) = {
    val usesBetweenMovedDefsViaThis =
      usesBetween(g, movedDef, movedDecl).filter(usesViaThis(g))

    val g1 =
     if(usesBetweenMovedDefsViaThis.nonEmpty && !(newSelfUse existsIn g))
        newSelfUse createIn g
      else g

    usesBetweenMovedDefsViaThis.foldLeft(g1){
      _.changeTypeUseOfTypeMemberUse(oldSelfUse, newSelfUse, _)
    }

  }

  def typeMemberBetweenUnrelatedTypeDecl
  ( g0 : DependencyGraph,
    typeMembersMovedId : List[NodeId],
    oldContainer : NodeId,
    newContainer : NodeId,
    createVarStrategy: Option[CreateVarStrategy] = None) : LoggedTG = {


    val movedDecl = typeMembersMovedId.toSet
    val siblings = g0.content(oldContainer) -- movedDecl

    val movedDef : Set[NodeId] =
      movedDecl map g0.definitionOf flatten

    val movedDefUsingSiblingViaThis : Set[NodeId] =
      usesBetween(g0, movedDef, siblings).filter(usesViaThis(g0)).map(_.user)

    val movedDeclWithArgUsableAsReceiver : Set[NodeId] =
      movedDecl.filter {
        nid =>
          g0.parametersOf(nid) exists (g0.styp(_).get uses newContainer)
      }

    val oldSelfUse = Uses(oldContainer, oldContainer)


    val g1 = typeMembersMovedId.foldLeft(g0){
      (g0, movedId) =>
        g0.changeSource(Contains(oldContainer, movedId), newContainer)
    }

    val g2 = adjustSelfUsesBR(g1, movedDecl, movedDef, oldSelfUse, Uses(newContainer, newContainer))

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
      if( (oldSelfUse existsIn g5) &&
        g5.typeMemberUsesOf(oldSelfUse).isEmpty)
        g5.removeEdge(oldSelfUse)
      else g5
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
        val params = g0 parametersOf used
        params find {g0.styp(_) contains NamedType(newContainer)} match {
          case None =>
            LoggedError(s"An arg typed ${NamedType(newContainer)} was expected")
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
  (g: DependencyGraph,
   declId : NodeId,
   pType : NodeId,
   oldTypeUse : Uses,
   tmUses : Set[Uses]
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


    val defNodeSet = declNodeSet map g.definitionOf filter (_.nonEmpty) map (_.get)

    val typeUses : Set[Uses] =
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
        LoggedError("create var strategy required")
      case None => LoggedSuccess(g)
    }
  }




  def createParam
  (g : DependencyGraph,
   someOldTypeUse : Uses,
   newTypeUsed : NodeId,
   tmUses : Set[Uses]
    ): LoggedTG ={

    val usesByUser = tmUses.groupBy(_.user)
    //introduce one parameter by user even with several uses
    //these were all previously this use so it makes sens to keep one reference
    val tmUsesStr = tmUses.map(u => (g,u).shows).mkString("[",", ","]")
    usesByUser.toList.foldLoggedEither(g.comment(s"createParam(g,${(g, someOldTypeUse).shows}, ${(g,newTypeUsed).shows}, $tmUsesStr)")) {
      case (g0, (impl, typeMemberUses)) =>
        val user = g0.getConcreteNode(impl)

        assert(g0.getConcreteNode(impl).kind.kindType == ValueDef)

        val decl = g0.getConcreteNode(g0.container_!(impl))

        addParamOfTypeAndSetTypeDependency(g0, decl.id,
          newTypeUsed, someOldTypeUse, typeMemberUses)

    }
  }

  def createTypeMember
  (g : DependencyGraph,
   oldTypeUse : Uses,
   newTypeUsed : NodeId,
   tmContainer : NodeId,
   tmUses : Set[Uses],
   kind : NodeKind
    ): LoggedTG ={
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
