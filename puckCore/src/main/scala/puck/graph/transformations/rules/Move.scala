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

import scalaz.std.list._
import scalaz.std.set._
import ShowDG._
import puck.PuckError

import scalaz._


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

        g.comment(s"Move.staticDecl(g, ${(g, movedId).shows}, ${(g, newContainer).shows})").
          changeSource(Contains(oldContainer, movedId), newContainer) logComment log
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
        g.changeTypeUseForTypeMemberUseSet(oldSelfUse, newSelfUse,
          usesBetweenMovedDefsViaThis)

      val g2 =
        if(g1.typeMemberUsesOf(newSelfUse).nonEmpty && !(newSelfUse existsIn g1))
          newSelfUse createIn g1
        else g1

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


        def findReceiver(userOfMovedDecl : NodeId) = {
          val log = s"\nsearching old receiver\n" +
            "userOfMovedDecl = " +  (g0, userOfMovedDecl ).shows(desambiguatedFullName)

          val candidates =
            (g0 usedBy userOfMovedDecl)  filter (g0.uses(_,oldContainer))

//          import scala.collection.mutable
//          val candidates =  mutable.Set[NodeId]()
//
//          val bounds = mutable.Set[NodeId]((g0 usedBy userOfMovedDecl).toSeq:_*)
//          bounds remove movedDecl
//          val visited = mutable.Set[NodeId]()
//          while(bounds.nonEmpty) { //should'nt we limit the loop to 2 iterations ??
//            val c = bounds.head
//            bounds remove c
//
//            visited add c
//            g0.usedBy(c) diff visited foreach bounds.add
//
//            if(g0.uses(c,typeOfReceiver) && c != typeOfReceiver)
//              candidates add c
//          }

          if (candidates.size != 1)
            LoggedError(s"$log\nfailure, candidates = " + (candidates map (c => (g0, c).shows)) )
          else LoggedSuccess(s"$log\nsuccess :  receiver =${(g0, candidates.head).shows}",
            candidates.head)
        }


        import puck.util.LoggedEither, LoggedEither._
        def useArgAsReceiver
        ( g: DependencyGraph/*,
          movedDeclWithArgUsableAsReceiver: Set[NodeId]*/
        ) : LoggedTG = {



          def findActualParameter(userOfMovedDecl : NodeId) = {
            val candidates =
              (g0 usedBy userOfMovedDecl) filter (g0.uses(_, newContainer))
            if (candidates.size != 1)
              LoggedError(s"useArgAsReceiver searching actual parameter failure : ${candidates.size} candidates")
            else LoggedSuccess(candidates.head)
          }

          val log = "use arg as receiver for " +
            (movedDeclWithArgUsableAsReceiver map (id => (g,id).shows(desambiguatedFullName)) mkString ("[ ", ", " ," ]"))

          def isReceiverTypeConstrainedIn(g : DependencyGraph, receiver : NodeId, ctxt : NodeId) : Boolean =
          g.kindType(receiver) match {
            case Parameter | InstanceValue =>
            g.typ(receiver).ids.exists { tid =>
              g.typeConstraints((receiver, tid)).exists(ct => ct.constrainedUser == ctxt )
            }
            case _ => //do not know but in doubt ...
              true
          }

          movedDeclWithArgUsableAsReceiver.foldLoggedEither(g logComment log){
            (g0, movedDecl) =>
              ( g0 parametersOf movedDecl ) find ( g0.typ(_) uses newContainer ) match {
                case None =>
                  LoggedError(s"An arg typed ${NamedType(newContainer)} was expected")
                case Some(pid) =>

                  val ltg = Remove.concreteNode(g0, pid)

                  val paramTypeUses = (pid, newContainer)

                  //inside the moved method, accesses qualified by the removed parameter
                  //are now qualified by this
                  val ltg2 = ltg map (_.changeTypeUseForTypeMemberUseSet( paramTypeUses, newSelfUse,
                    g0 typeMemberUsesOf paramTypeUses ))

                  import LoggedEither.loggedEitherMonad
                  (g0 usersOf movedDecl).foldLoggedEither(ltg2){
                    (g2, user) =>
                      Apply[LoggedTry].apply2(findReceiver(user),
                        findActualParameter(user)){(receiver, actualParam) =>

                        val g3 = g2.changeTypeUseOfTypeMemberUse((receiver, oldContainer),
                          (actualParam, newContainer),
                          (user,movedDecl))

                        val otherUsesQualifiedByReceiver =
                          g3.typeMemberUsesOf(receiver, oldContainer).filter(_.user == user)


                        val qualifierWillBeUsedAsArg = {
                          val someMovedDef = g0.definitionOf(movedDecl)
                          someMovedDef.nonEmpty &&
                            usesOfSiblingViaThis.exists(uses => uses.user == someMovedDef.get)
                        }

                        val isTypedConstrained =
                          isReceiverTypeConstrainedIn(g3, receiver, user)
                        // meaning it appears as plain ref, as rvalue, parameter or in return stmt
                        // use should not be deleted

                        if(otherUsesQualifiedByReceiver.isEmpty &&
                          ! qualifierWillBeUsedAsArg &&
                          ! isTypedConstrained)
                          g3.typeUsesOf((user, receiver))
                            .foldLeft(g3.removeEdge(Uses(user, receiver))){
                              (g, tu) => g.removeBinding(tu, (user, receiver))
                            }
                        else g3
                      }

                  }


              }
          }
        }

        def useReceiverAsArg
        ( g: DependencyGraph/*,
          usesOfSiblingViaThis : Set[NodeIdP]*/
        ) : LoggedTG = {
          //this is the moved nodes that uses their unmoved siblings !
          val usersOfSiblingViaThis = usesOfSiblingViaThis.groupBy(_.user)
          val log = "\nuse receiver as arg for " +
            (usersOfSiblingViaThis.keys map (id => (g,id).shows(desambiguatedFullName)) mkString ("[ ", ", " ," ]\n"))

          usersOfSiblingViaThis.toList.foldLoggedEither(g logComment log){
            case (g0, (movedDef, siblingUses)) =>
              val decl = g0.container_!(movedDef)

              addParamOfTypeAndSetTypeDependency(g0, decl,
                oldContainer, oldSelfUse, siblingUses) flatMap  {
                case (pnode, g1) =>
                  g1.usersOf(g1 container_! movedDef).foldLoggedEither(g1) {
                    case (g00, user) =>
                      findReceiver(user) map { receiver =>
                        g00.addTypeUsesConstraint((pnode.id, oldContainer),
                          Sub((receiver, oldContainer)))}
                  }
            }
          }
        }

        def createNewReceiver
        ( g: DependencyGraph,
          newContainer : NodeId/*,
          usesThatRequireNewReceiver : Set[NodeIdP]*/,
          createVarStrategy: CreateVarStrategy) : LoggedTG = {

          def singleTypeUse(tmUse : NodeIdP) : NodeIdP = {
            val tus =  g.typeUsesOf(tmUse)
            if(tus.size > 1) error("require type member uses with only one type use")
            else tus.head
          }

          try {
            val typeUses = usesThatRequireNewReceiver.groupBy(singleTypeUse)

            val log = "create new receiver for " +
              (typeUses.keys map (id => (g,id).shows) mkString ("[", "," ,"]"))


            typeUses.toList.foldLoggedEither(g.logComment(log)) {
              case (g0, (tu, tmUses)) =>
                createVarStrategy(g0, tu, newContainer, tmUses)
            }
          }catch {
            case e : PuckError => LoggedError(e)
            case e : NoSuchElementException => LoggedError(e.getMessage)
          }
        }


        val movedDeclWithArgUsableAsReceiver: Set[NodeId] =
          for { nid <- movedDecls
            if g0.parametersOf(nid) exists (g0.typ(_) uses newContainer)
          } yield nid

        val usesThatRequireNewReceiver = for {
          declId <- movedDecls -- movedDeclWithArgUsableAsReceiver
          user <- g0 usersOf declId
          if !(movedDefs contains user)
        } yield (user, declId)

        def apply() : LoggedTG = {

          for {
            g2 <- useArgAsReceiver(g1/*, movedDeclWithArgUsableAsReceiver*/)

            g3 <- useReceiverAsArg(g2/*, usesOfSiblingViaThis*/)

            g4 <- (usesThatRequireNewReceiver.nonEmpty, createVarStrategy) match {
              case (true, Some(strategy)) =>
                createNewReceiver(g3, newContainer, /*usesThatRequireNewReceiver,*/ strategy)
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
  ) : LoggedTry[(ConcreteNode, DependencyGraph)]  = {
    import g.nodeKindKnowledge.intro
    intro.parameter(g, pType, declId) map {
      case (pNode, g2) =>
        val newTypeUse = (pNode.id, pType)
        (pNode, g2.changeTypeUseForTypeMemberUseSet(oldTypeUse, newTypeUse, tmUses))
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
          newTypeUsed, oldTypeUse, typeMemberUses) map (_._2)

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
