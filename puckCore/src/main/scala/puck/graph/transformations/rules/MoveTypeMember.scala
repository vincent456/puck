package puck.graph.transformations.rules

import puck.PuckError
import puck.graph._
import puck.util.LoggedEither._

import scalaz.{Arrow => _, _}, Scalaz._ //hide arrow
import ShowDG._

sealed trait CreateVarStrategy {

  def moveTypeMemberUsedBySelf
  (g: DependencyGraph,
   currentContainer: NodeId,
   newContainer: NodeId,
   siblingsUsesViaSelf: List[Uses],
   intro : Intro): LoggedTG

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
   currentContainer: NodeId,
   newContainer: NodeId,
   siblingsUsesViaSelf: List[Uses],
   intro : Intro): LoggedTG = {

    val usesByUser = siblingsUsesViaSelf.groupBy(_.user)
    //introduce one parameter by user even with several uses
    //these were all previously this use so it makes sens to keep one reference
    usesByUser.toList.foldLoggedEither(g) {
      case (g0, (userId, typeMemberUses)) =>
        val user = g0.getConcreteNode(userId)
        val newTypeUse = Uses(userId, newContainer)
        (g0.kindType(user), user.styp) match {
          case (TypeMember, Some(NamedType(_))) => ???

          case (TypeMember, Some(ar@Arrow(_, _))) =>
            val log = s"${showDG[NodeId](g0).shows(userId)}, user of moved method" +
              s" will now use ${showDG[NodeId](g0).shows(newContainer)}"

            val g1 = g0.setType(userId, Some(ar.prependParameter(NamedType(newContainer))))
              .addUses(userId, newContainer)

            val g2 = typeMemberUses.foldLeft(g1) {
              (g00, typeMemberUse) =>
                g00.typeUsesOf(typeMemberUse).foldLeft(g00){
                  (g000, typeUse) =>
                    removeUsesDependencyTowardSelfUse(g000, typeUse, typeMemberUse)
                      .addUsesDependency(newTypeUse, typeMemberUse)
                }
            }

            LoggedSuccess(g2, log)

          case (_, _) => LoggedError(new PuckError(s"a type member was expected"))
        }
    }
  }
}

case class CreateTypeMember(kind : NodeKind) extends CreateVarStrategy {

  def createDelegateUses
  ( currentContainer: NodeId,
    newContainer: NodeId,
    getDelegate : DependencyGraph => (NodeId, DependencyGraph) )
  ( g : DependencyGraph,
    typeMemberUse : Uses,
    typeUse : DGUses
    ) : (NodeId, DependencyGraph) = {

    val g2 = removeUsesDependencyTowardSelfUse(g, typeUse, typeMemberUse)

    val (delegate, g3) = getDelegate(g2)

    val thisUse = Uses(typeMemberUse.user, currentContainer)

    //addUsesDependency done before addUse to have some context when applying
    val g4 = g3.addUsesDependency(Uses(delegate, newContainer), typeMemberUse)
      .addUses(delegate, newContainer) //type field

    val g5 = thisUse.changeTarget(g4, delegate) // replace this.m by delegate.m

    (delegate, g5)
  }

  def moveTypeMemberUsedBySelf
  (g: DependencyGraph,
   currentContainer: NodeId,
   newContainer: NodeId,
   siblingsUsesViaSelf: List[Uses], // used -> users
   intro : Intro): LoggedTG ={

    type GenDelegate = DependencyGraph => (NodeId, DependencyGraph)

    def genDelegate : GenDelegate = {
      g =>
        val delegateName = s"${g.getConcreteNode(newContainer).name.toLowerCase}_delegate"

        val (field, g2) =
          intro.accessToType(g, delegateName, kind, newContainer)
        (field.id, g2.addContains(currentContainer, field.id))
        
//        val (field, g2) =
//          intro(g, delegateName, k, Some(NamedType(newContainer)))
//        (field.id, g2.addContains(currentContainer, field.id))
    }

    val selfTypeUses = DGEdge.UsesK(currentContainer, currentContainer)

    val (delegate, g2) =
      createDelegateUses(currentContainer, newContainer, genDelegate)(
        g, siblingsUsesViaSelf.head, selfTypeUses)

    def getDelegate : GenDelegate = g => (delegate, g)

    siblingsUsesViaSelf.tail.foldLoggedEither(g2) {
      case (g0, uses) =>
        g0.typeUsesOf(uses).foldLoggedEither(g0){
          (g0, typeUse) =>
            LoggedSuccess(createDelegateUses(currentContainer, newContainer,getDelegate)(g0, uses, typeUse)._2)
        }
    }

  }
}


