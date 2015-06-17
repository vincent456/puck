package puck.graph.transformations.rules

import puck.PuckError
import puck.graph._
import puck.graph.constraints.{AbstractionPolicy, NotAnAbstraction, RedirectionPolicy, DelegationAbstraction}
import puck.util.LoggedEither, LoggedEither._
import scalaz._, Scalaz._
import ShowDG._

object Redirection {

  def redirectUses(g : DependencyGraph,
                   oldEdge : DGEdge, newUsed : NodeId,
                   keepOldUse : Boolean) : DependencyGraph = {

    val g3 = if(keepOldUse) g.addUses(oldEdge.user, newUsed)
    else oldEdge.changeTarget(g, newUsed)

    g3.changeType(oldEdge.user, oldEdge.used, newUsed)
  }


  def redirectUsesAndPropagate(g  : DependencyGraph,
                               oldUse : DGUses,
                               newUsed : Abstraction,
                               propagateRedirection : Boolean/* = true*/,
                               keepOldUse : Boolean /*= false */) : LoggedTG =
    (newUsed, oldUse.accessKind)  match {
      case (AccessAbstraction(absId, pol), _) =>
        redirectUsesAndPropagate(g, oldUse, absId, pol, propagateRedirection, keepOldUse)
      case (ReadWriteAbstraction(Some(rid), _), Some(Read)) =>
        redirectUsesAndPropagate(g, oldUse, rid, DelegationAbstraction, propagateRedirection, keepOldUse)
      case (ReadWriteAbstraction(_, Some(wid)), Some(Write)) =>
        redirectUsesAndPropagate(g, oldUse, wid, DelegationAbstraction, propagateRedirection, keepOldUse)
      case (ReadWriteAbstraction(Some(rid), Some(wid)), Some(RW)) =>
        redirectUsesAndPropagate(g, oldUse, rid, DelegationAbstraction, propagateRedirection, keepOldUse = true).flatMap {
          redirectUsesAndPropagate(_, oldUse, wid, DelegationAbstraction, propagateRedirection, keepOldUse)
        }
      case _ => sys.error("should not happen")
      }

  def redirectUsesAndPropagate(g  : DependencyGraph,
                               oldUse : DGUses,
                               newUsed : NodeId,
                               policy : RedirectionPolicy,
                               propagateRedirection : Boolean/* = true*/,
                               keepOldUse : Boolean /*= false */) : LoggedTG = {
    val log0 = s"redirectUsesAndPropagate(_, oldUse = $oldUse, newUsed = $newUsed, $policy, " +
      s"propagate = $propagateRedirection, keepOldUse = $keepOldUse)"
    val log1 = s"\n$oldUse.exists = ${oldUse.existsIn(g)}\n"
    val lg1 = (g logComment log0) :++> log1

    if(oldUse.used == newUsed) lg1.toLoggedEither
    else if(oldUse.existsIn(g)){
      val lg2 = lg1 :++> s"redirecting ${showDG[DGEdge](g).show(oldUse)} target " +
        s"to ${showDG[NodeId](g).show(newUsed)} ($policy)\n"

      if (!propagateRedirection)
        lg2.map(redirectUses(_, oldUse, newUsed, keepOldUse)).toLoggedEither
      else {

        val oldUsedKind = g.kindType(oldUse.used)
        val ltg : LoggedTG = (lg2 :++> s"use target is a $oldUsedKind\n").toLoggedEither

        val propagateTypeMember : DependencyGraph => LoggedTG =
          redirectTypeUsesOfTypeMemberUse(_, oldUse, newUsed, policy)

        val propagateType : DependencyGraph => LoggedEither[PuckError, (KeepOldTypeUse, DependencyGraph)] =
          redirectTypeMemberAndConstructorUsesOfTypeUse(_, oldUse, newUsed, policy)


        val ltg2 : LoggedTG = (oldUsedKind match {
          case TypeConstructor | TypeMember =>
            ltg.flatMap(propagateTypeMember(_)).map((false,_))
          case TypeDecl => ltg.flatMap(propagateType(_))
          case TypeDeclAndTypeMember =>
            ltg.flatMap(propagateTypeMember(_)).flatMap(propagateType)
          case _ => ltg.map((false,_))
        }) map { case (keep, g2) =>
          redirectUses(g2, oldUse, newUsed, keep || keepOldUse)
        }

        oldUsedKind match {
          case TypeConstructor =>
            val ctorId = oldUse.used
            g.container(ctorId) match {
              case None =>
                ltg2.error(new PuckError("constructor should have a container"))

              case Some(classId) =>
                ltg2 map { g0 =>
                g.usersOf(oldUse.user).foldLeft(g0){ case (g1, userId) =>
                  g1.addUses(userId, ctorId).addUses(userId, classId)}
              }
            }
          case _ => ltg2
        }

      }
    }
    else if (g.uses(oldUse.user, newUsed)) {
      //if m uses  both A and A.mi, the first uses dominate the second
      //if both are identified as violations and are in a wrongusers list
      //redirecting the one will redirect the other
      // when iterating on the wrongusers, the next call to redirectuses will arrive here
      lg1 :++> s"redirecting uses $oldUse target to $newUsed ($policy) : " +
        s"FAILURE !! ${oldUse.used}} is not used\n"
      lg1.toLoggedEither
    }
    else if(g.usersOf(oldUse.used).contains(oldUse.user) ||
      g.usersOf(newUsed).contains(oldUse.user))
      lg1.toLoggedEither[PuckError].error(new DGError("incoherent state !!!!!!!!!!!!"))
    else
      lg1.toLoggedEither[PuckError].error(new DGError(s"redirecting uses ${showDG[DGEdge](g).show(oldUse)} target to ${showDG[NodeId](g).show(newUsed)} ($policy)\n" +
        s"!!! nor the oldUsee or the newUsee is really used !!! "))

  }

  def redirectTypeUsesOfTypeMemberUse(g : DependencyGraph,
                                      currentTypeMemberUse : DGUses,
                                      newTypeMemberUsed : NodeId,
                                      policy : RedirectionPolicy,
                                      propagateRedirection : Boolean = true) : LoggedTG = {

    val log = s"redirecting Type uses of typeMember use ${showDG[DGEdge](g).shows(currentTypeMemberUse)}" +
      s" (new typeMember used is ${showDG[NodeId](g).shows(newTypeMemberUsed)})\n"
    val lg = g logComment log

    val typeUses = g.typeUsesOf(currentTypeMemberUse)
    if(typeUses.isEmpty)
      (lg :++> "no primary uses to redirect\n").toLoggedEither
    else{
      val lg1 =
        lg :++> "uses to redirect:%s".format(typeUses.mkString("\n\t", "\n\t","\n"))

      typeUses.foldLoggedEither[PuckError, DependencyGraph](lg){
        (g0, typeUse) =>
          val keepOldUse = g.typeMemberUsesOf(typeUse).tail.nonEmpty //is empty if typeUses had only one side use

          val redirect : DependencyGraph => LoggedTry[(DGUses, DependencyGraph)] =
            if(typeUse.selfUse) g => ??? //redirectThisTypeUse(g, typeUses.used, newTypeMemberUsed)
            else {
              g =>
                findNewTypeUsed(g, typeUse.used, newTypeMemberUsed, policy).flatMap {
                  case (g00, newTypeUsed) =>
                    redirectUsesAndPropagate(g00, typeUse, newTypeUsed,
                      policy, propagateRedirection, keepOldUse).map{(Uses(typeUse.user, newTypeUsed),_)}
                }
            }

          val g1 = g.removeUsesDependency(typeUse, currentTypeMemberUse)

          for(
            eg <- redirect(g1);
            (newTypeUse, g2) = eg
          ) yield
          g2.addUsesDependency(newTypeUse, Uses(currentTypeMemberUse.user, newTypeMemberUsed))

      }
    }
  }


  def findNewTypeUsed
  ( g : DependencyGraph,
    currentTypeUsed : NodeId,
    newTypeMemberUsed : NodeId,
    policy : RedirectionPolicy
    ) : LoggedTry[(DependencyGraph, NodeId)] = {

    val log = s"searching new Type used ($policy) : current type used is " +
      s"${showDG[NodeId](g).shows(currentTypeUsed)}, new typeMember used : ${showDG[NodeId](g).shows(newTypeMemberUsed)}\n"
    val lg= g logComment log

    def logFind(newPrimaryUsed : NodeId) =
      (lg :++> s"new type to use found : ${showDG[NodeId](g).shows(newPrimaryUsed)}\n").map{
        (_, newPrimaryUsed)}.toLoggedTry

    policy match {
        case NotAnAbstraction =>
          logFind(g.container(newTypeMemberUsed).get)
        case absPolicy : AbstractionPolicy =>
          g.abstractions(currentTypeUsed).find {
            case AccessAbstraction(node, `absPolicy`) => g.contains_*(node, newTypeMemberUsed)
            case _ => false
          } match {
            case Some(AccessAbstraction(n, _)) => logFind(n)
            case Some(_) => sys.error("cannot happen")
            case None =>
              val abstractKinds =
                g.getNode(currentTypeUsed).kind.
                  abstractionNodeKinds(absPolicy)

              g.nodesId.find{ node =>
                g.contains_*(node, newTypeMemberUsed) && {
                  abstractKinds.contains(g.getNode(node).kind)
                }

              } match {
                case Some(newPrimaryUsed) =>
                  logFind(newPrimaryUsed)
                case None =>
                  LoggedError(new RedirectionError("no correct primary abstraction found !"))
              }
          }
      }

    }




  type TypeMemberUses = List[DGUses]
  def redirectTypeMemberUsesOfTypeUse
    (g : DependencyGraph,
     currentTypeUse: DGUses,
     newTypeUsed : NodeId,
     policy : RedirectionPolicy,//TODO remove arg ?
     tmu : TypeMemberUses) : LoggedTG = {

    val log = s"redirecting typeMember uses of type use ${showDG[DGEdge](g).shows(currentTypeUse)} " +
      s"(new type used is  ${showDG[NodeId](g).shows(newTypeUsed)})"

    val log1 = if (tmu.isEmpty) "no typeMember uses to redirect\n"
    else "uses to redirect:%s".format(tmu.mkString("\n\t", "\n\t", "\n"))

    val lg : LoggedG = (g logComment log) :++> ("\n" + log1)

    tmu.foldLoggedEither[PuckError, DependencyGraph](lg) {
      case (g0 : DependencyGraph, typeMemberUse) =>
        val typeMember = typeMemberUse.used
        val someTypeMemberAbs =
          g.abstractions(typeMember).find { abs =>
            assert(abs.toList.nonEmpty)
            abs.toList.forall(g.contains(newTypeUsed, _))
          }

        someTypeMemberAbs match {
          case None =>
            val msg = s"While redirecting type use ${showDG[DGEdge](g).shows(currentTypeUse)} " +
              s"target to ${showDG[NodeId](g).shows(newTypeUsed)}\n" +
              s"no satisfying abstraction to redirect typeMember use ${showDG[DGEdge](g).shows(typeMemberUse)}"

            g0.set(msg+"\n").toLoggedEither[PuckError].error(new RedirectionError(msg))

          case Some(abs) =>
            redirectUsesAndPropagate(
              g0.removeUsesDependency(currentTypeUse, typeMemberUse),
              typeMemberUse, abs,
              propagateRedirection = true,
              keepOldUse = false).map {
                g2 =>
                  abs.toList.foldLeft(g2){
                    (g0, absId) =>
                      val newSide = Uses(typeMemberUse.user, absId)
                      g2.addUsesDependency(Uses(currentTypeUse.user, newTypeUsed), newSide)
                  }


              }
        }
    }

  }



  type KeepOldTypeUse = Boolean
  def redirectTypeMemberAndConstructorUsesOfTypeUse
  ( g : DependencyGraph,
    currentTypeUse: DGUses,
    newTypeUsed : NodeId,
    policy : RedirectionPolicy
    ): LoggedEither[PuckError, (KeepOldTypeUse, DependencyGraph)] = {

    val log = s"redirecting typeMember AND CONSTRUCTOR uses of type use ${showDG[DGEdge](g).shows(currentTypeUse)} " +
      s"(new type used is  ${showDG[NodeId](g).shows(newTypeUsed)})"
    val lg = (g logComment log) :++> "\n"


    val typeMemberAndTypeCtorUses = g.typeMemberUsesOf(currentTypeUse).toList

    import puck.util.Collections.SelectList

    def newTypeUsedHasAccessAbstractionOf(nId : NodeId) : Boolean =
      g.abstractions(nId).exists {
        case AccessAbstraction(abs, _) => g.contains(newTypeUsed, abs)
        case _ => false
      }

    val redirect : DependencyGraph => LoggedEither[PuckError, (KeepOldTypeUse, DependencyGraph)] =
    typeMemberAndTypeCtorUses.select { e => g.kindType(g.getNode(e.target)) == TypeConstructor} match {
      case Some((typeCtorUse, typeMemberUses))
        if ! newTypeUsedHasAccessAbstractionOf(typeCtorUse.used) =>
        redirectTypeMemberUsesOfTypeUse(_, currentTypeUse, newTypeUsed, policy, typeMemberUses).map((true, _))
      case _ =>
        redirectTypeMemberUsesOfTypeUse(_, currentTypeUse, newTypeUsed, policy, typeMemberAndTypeCtorUses).map((false, _))
    }

    lg.toLoggedEither[PuckError] flatMap redirect
  }
}
