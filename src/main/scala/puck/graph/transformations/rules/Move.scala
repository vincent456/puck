package puck.graph.transformations.rules

import puck.PuckError
import puck.graph.ShowDG._
import puck.graph._

import puck.util.Collections.traverse
import puck.graph.transformations.rules.Redirection.redirectUses

import scalaz.{\/-, -\/}


sealed trait CreateVarStrategy {

  def moveTypeMemberUsedBySelf
  (g: DependencyGraph,
   typeMemberMoved: ConcreteNode,
   currentContainer: NodeId,
   newContainer: NodeId,
   siblingsUserViaSelf: Set[NodeId],
   intro : Intro): Try[DependencyGraph]


  protected def removeUsesDependencyTowardSelfUse
  ( g : DependencyGraph,
    selfTypeUse: DGUses,
    typeMemberUse : DGUses ) : DependencyGraph = {
    val g1 = g.removeUsesDependency(selfTypeUse, typeMemberUse)
    val keepOldUse = g1.typeMemberUsesOf(selfTypeUse).nonEmpty

    if (!keepOldUse) selfTypeUse deleteIn g1
    else g1
  }
}

case object CreateParameter extends CreateVarStrategy {

  def moveTypeMemberUsedBySelf
  (g: DependencyGraph,
   typeMemberMoved: ConcreteNode,
   currentContainer: NodeId,
   newContainer: NodeId,
   siblingsUserViaSelf: Set[NodeId],
   intro : Intro): Try[DependencyGraph] ={

    traverse(siblingsUserViaSelf, g) {
      case (g0, userId) =>

        traverse(g.typeUsesOf(userId, typeMemberMoved.id), g0){ (g0, typeUse) =>

          val typeMemberUse = DGEdge.UsesK(userId, typeMemberMoved.id)
          val g2 = removeUsesDependencyTowardSelfUse(g0, typeUse, typeMemberUse)

          val user = g2.getConcreteNode(userId)
          (g2.kindType(user), user.styp) match {
            case (TypeMember, Some(NamedType(_))) => ???

            case (TypeMember, Some(ar @ Arrow(_, _))) =>
              g2.logger.writeln(s"${showDG[NodeId](g2).shows(userId)}, user of moved method" +
                s" will now use ${showDG[NodeId](g2).shows(newContainer)}")

              val g3 = g2.setType(userId, Some(Arrow(NamedType(newContainer) , ar)))
                .addUses(userId, newContainer)
                .addUsesDependency(Uses(userId, newContainer), typeMemberUse)
              \/-(g3)

            case (_, _) => -\/(new PuckError(s"a type member was expected"))
          }

        }

    }
  }

}

case class CreateTypeMember(k : NodeKind) extends CreateVarStrategy {

  def moveTypeMemberUsedBySelf
  (g: DependencyGraph,
   typeMemberMoved: ConcreteNode,
   currentContainer: NodeId,
   newContainer: NodeId,
   siblingsUserViaSelf: Set[NodeId],
   intro : Intro): Try[DependencyGraph] ={

    val sibling = siblingsUserViaSelf.head

    type GenDelegate = DependencyGraph => (NodeId, DependencyGraph)

    def genDelegate : GenDelegate = {
      g =>
        val (field, g2) =
          intro.createNode(g, s"${typeMemberMoved.name}_delegate", k, Some(NamedType(newContainer)))
        (field.id, g2.addContains(currentContainer, field.id))
    }


    def createDelegateUses(getDelegate : DependencyGraph => (NodeId, DependencyGraph) )
                          (g : DependencyGraph,
                           userId : NodeId,
                           typeUse : DGUses) : (NodeId, DependencyGraph) = {

      val typeMemberUse = DGEdge.UsesK(userId, typeMemberMoved.id)
      val g2 = removeUsesDependencyTowardSelfUse(g, typeUse, typeMemberUse)

      val (delegate, g3) = getDelegate(g2)

      val thisUse = DGEdge.UsesK(userId, currentContainer)

      //addUsesDependency done before addUse to have some context when applying
      val g4 = g3.addUsesDependency(Uses(delegate, newContainer), typeMemberUse)
        .addUses(delegate, newContainer) //type field

      val g5 = thisUse.changeTarget(g4, delegate) // replace this.m by delegate.m

      (delegate, g5)
    }

    val selfTypeUses = DGEdge.UsesK(currentContainer, currentContainer)
    val (delegate, g2) = createDelegateUses(genDelegate)(g, sibling, selfTypeUses)

    def getDelegate : GenDelegate = g => (delegate, g)

    traverse(siblingsUserViaSelf.tail, g2) {
      case (g0, userId) =>
        traverse(g0.typeUsesOf(userId, typeMemberMoved.id), g0){
          (g0, typeUse) =>
            \/-(createDelegateUses(getDelegate)(g0, userId, typeUse)._2)
        }
    }

  }
}

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


  private def siblingTest[T]
  (f : (Set[NodeId], NodeId => Boolean) => T)
  ( graph : DependencyGraph,
    toBeMoved : DGNode,
    currentHost : DGNode
    ) : T = {
    def sibling: NodeId => Boolean =
      sid => graph.contains(currentHost.id, sid) && sid != toBeMoved.id

    def siblingUserViaSelf: NodeId => Boolean =
      user =>
      sibling(user) && {
        val tuses = graph.typeUsesOf(user, toBeMoved.id)
        tuses.exists(_.selfUse)
      }

    f(graph.usersOf(toBeMoved.id), siblingUserViaSelf)

  }


  def isUsedBySiblingsViaSelf = siblingTest(_.exists(_)) _

  def redirectTypeUsesOfMovedTypeMemberUsers
  ( g : DependencyGraph,
    typeMemberMoved : NodeId,
    newTypeUsed : NodeId,
    users : Set[NodeId]
    ) : DependencyGraph = {

          users.foldLeft(g) {
            case (g0, userId) =>

              val typeMemberUse = DGEdge.UsesK(userId, typeMemberMoved)
              
              g.typeUsesOf(typeMemberUse).foldLeft(g0) { (g0, typeUse) =>

                val g1 = g0.removeUsesDependency(typeUse, typeMemberUse)
                val keepOldUse = g1.typeMemberUsesOf(typeUse).nonEmpty

                val newTypeUse = DGEdge.UsesK(typeUse.user, newTypeUsed)

                redirectUses(g1, typeUse, newTypeUsed, keepOldUse)
                  .addUsesDependency(newTypeUse, Uses(userId, typeMemberMoved))
              }
          }

    }


  private def logTypeMemberMove
  ( g : DependencyGraph,
    typeMemberMoved : NodeId,
    oldContainer : NodeId,
    newContainer : NodeId
    ) : DependencyGraph = {
    val log = s"moving type member ${showDG[NodeId](g).shows(typeMemberMoved)} " +
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

  def typeMember(g0 : DependencyGraph,
                 typeMemberMovedId : NodeId, newContainer : NodeId,
                 createVarStrategy: Option[CreateVarStrategy] = None): Try[DependencyGraph] = {

    val oldContainer = g0.getConcreteNode(g0.container(typeMemberMovedId).get)
    val typeMemberMoved = g0.getConcreteNode(typeMemberMovedId)

    val g = logTypeMemberMove(g0, typeMemberMoved.id, oldContainer.id, newContainer)

    val g2 = g.changeSource(DGEdge.ContainsK(oldContainer.id, typeMemberMoved.id), newContainer)
    val (siblingsUserViaSelf, otherUsers) =
      siblingTest(_.partition(_))(g2, typeMemberMoved, oldContainer)

    val tg : Try[DependencyGraph] = (siblingsUserViaSelf.nonEmpty, createVarStrategy) match {
      case (true, Some(strategy)) =>
        strategy.moveTypeMemberUsedBySelf(g2, typeMemberMoved,
          oldContainer.id, newContainer, siblingsUserViaSelf, intro)
      case (false, None) => \/-(g2)
      case _ => -\/(new PuckError("incoherent call to Move.typMember"))
    }
    tg.map(redirectTypeUsesOfMovedTypeMemberUsers(_, typeMemberMoved.id, newContainer, otherUsers))
  }

}
