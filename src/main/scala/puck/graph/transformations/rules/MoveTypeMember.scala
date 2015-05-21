package puck.graph.transformations.rules

import puck.PuckError
import puck.graph.ShowDG._
import puck.graph._
import puck.util.Collections._

import scalaz.{-\/, \/-}


  sealed trait CreateVarStrategy {

    def moveTypeMemberUsedBySelf
    (g: DependencyGraph,
     currentContainer: NodeId,
     newContainer: NodeId,
     siblingsUsesViaSelf: Seq[Uses],
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
     currentContainer: NodeId,
     newContainer: NodeId,
     siblingsUsesViaSelf: Seq[Uses],
     intro : Intro): Try[DependencyGraph] = {

      val usesByUser = siblingsUsesViaSelf.groupBy(_.user)
      //introduce one parameter by user even with several uses
      //these were all previously this use so it makes sens to keep one reference
      traverse(usesByUser, g) {
        case (g0, (userId, typeMemberUses)) =>

          val user = g0.getConcreteNode(userId)
          val newTypeUse = Uses(userId, newContainer)
          (g0.kindType(user), user.styp) match {
            case (TypeMember, Some(NamedType(_))) => ???

            case (TypeMember, Some(ar@Arrow(_, _))) =>
              g0.logger.writeln(s"${showDG[NodeId](g0).shows(userId)}, user of moved method" +
                s" will now use ${showDG[NodeId](g0).shows(newContainer)}")

              val g1 = g0.setType(userId, Some(Arrow(NamedType(newContainer), ar)))
                .addUses(userId, newContainer)

              val g2 = typeMemberUses.foldLeft(g1) {
                (g00, typeMemberUse) =>
                  g00.typeUsesOf(typeMemberUse).foldLeft(g00){
                    (g000, typeUse) =>
                      removeUsesDependencyTowardSelfUse(g000, typeUse, typeMemberUse)
                        .addUsesDependency(newTypeUse, typeMemberUse)
                  }
              }

              \/-(g2)

            case (_, _) => -\/(new PuckError(s"a type member was expected"))
          }
      }
    }
  }

  case class CreateTypeMember(k : NodeKind) extends CreateVarStrategy {

    def moveTypeMemberUsedBySelf
    (g: DependencyGraph,
     currentContainer: NodeId,
     newContainer: NodeId,
     siblingsUsesViaSelf: Seq[Uses], // used -> users
     intro : Intro): Try[DependencyGraph] ={

      type GenDelegate = DependencyGraph => (NodeId, DependencyGraph)

      def genDelegate : GenDelegate = {
        g =>
          val delegateName = s"${g.getConcreteNode(newContainer).name.toLowerCase}_delegate"
          val (field, g2) =
            intro.createNode(g, delegateName, k, Some(NamedType(newContainer)))
          (field.id, g2.addContains(currentContainer, field.id))
      }


      def createDelegateUses(getDelegate : DependencyGraph => (NodeId, DependencyGraph) )
                            (g : DependencyGraph,
                             typeMemberUse : Uses,
                             typeUse : DGUses) : (NodeId, DependencyGraph) = {

        val g2 = removeUsesDependencyTowardSelfUse(g, typeUse, typeMemberUse)

        val (delegate, g3) = getDelegate(g2)

        val thisUse = DGEdge.UsesK(typeMemberUse.user, currentContainer)

        //addUsesDependency done before addUse to have some context when applying
        val g4 = g3.addUsesDependency(Uses(delegate, newContainer), typeMemberUse)
          .addUses(delegate, newContainer) //type field

        val g5 = thisUse.changeTarget(g4, delegate) // replace this.m by delegate.m

        (delegate, g5)
      }

      val selfTypeUses = DGEdge.UsesK(currentContainer, currentContainer)
      val (delegate, g2) = createDelegateUses(genDelegate)(g, siblingsUsesViaSelf.head, selfTypeUses)

      def getDelegate : GenDelegate = g => (delegate, g)

      traverse(siblingsUsesViaSelf.tail, g2) {
        case (g0, uses) =>
          traverse(g0.typeUsesOf(uses), g0){
            (g0, typeUse) =>
              \/-(createDelegateUses(getDelegate)(g0, uses, typeUse)._2)
          }
      }

    }
  }


