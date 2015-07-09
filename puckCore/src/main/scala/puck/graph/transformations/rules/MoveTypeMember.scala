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

          case (TypeMember, Some(ar : Arrow )) =>
            val log = s"${showDG[NodeId](g0).shows(userId)}, user of moved method" +
              s" will now use ${showDG[NodeId](g0).shows(newContainer)}"

            val g1 = g0.setType(userId, Some(ar.prependParameter(NamedType(newContainer))))
              .addUses(userId, newContainer)

            val selfUse = Uses(currentContainer,currentContainer)

            val g2 = typeMemberUses.foldLeft(g1) {
              (g00, typeMemberUse) =>
                assert(g00.typeUsesOf(typeMemberUse) contains selfUse)

                removeUsesDependencyTowardSelfUse(g00, selfUse, typeMemberUse)
                   .addUsesDependency(newTypeUse, typeMemberUse)
            }

            LoggedSuccess(g2, log)

          case (_, _) => LoggedError(new PuckError(s"a type member was expected"))
        }
    }
  }
}

case class CreateTypeMember(kind : NodeKind) extends CreateVarStrategy {

  def moveTypeMemberUsedBySelf
  (g: DependencyGraph,
   currentContainer: NodeId,
   newContainer: NodeId,
   siblingsUsesViaSelf: List[Uses], // used -> users
   intro : Intro): LoggedTG ={

   val delegateName = s"${g.getConcreteNode(newContainer).name.toLowerCase}_delegate"

   val (delegate, g1) = intro.accessToType(g, delegateName, kind, newContainer)

   val g2 = g1.addContains(currentContainer, delegate.id)
                 .addUses(delegate.id, newContainer) //type field

   val oldTypeUse = Uses(currentContainer,currentContainer)
   val newTypeUse =  Uses(delegate.id, newContainer)

   siblingsUsesViaSelf.foldLoggedEither(g2) {
      case (g0, typeMemberUse) =>
        assert(g0.typeUsesOf(typeMemberUse) contains oldTypeUse)

        val g01 = removeUsesDependencyTowardSelfUse(g0, oldTypeUse, typeMemberUse)
                    .addUsesDependency(newTypeUse, typeMemberUse)

        val thisUse = Uses(typeMemberUse.user, currentContainer)

        LoggedSuccess(thisUse.changeTarget(g01, delegate.id)) // replace this.m by delegate.m
   }

  }
}


