package puck.graph.transformations.rules

import puck.PuckError
import puck.graph.ShowDG._
import puck.graph._
import puck.graph.constraints.{NotAnAbstraction, AbstractionPolicy, RedirectionPolicy}
import puck.util.PuckLog
import puck.util.Collections._
import scalaz._, Scalaz._

object Redirection {

  implicit class GOps(g : DependencyGraph) extends AnyVal{
    def logComment(msg : String) : LoggedG =
      g.comment(msg) set ("\n" + msg)
  }



  def redirectUses(g : DependencyGraph,
                   oldEdge : DGEdge, newUsed : NodeId,
                   keepOldUse : Boolean) : DependencyGraph = {

    val g3 = if(keepOldUse) g.addUses(oldEdge.user, newUsed)
    else oldEdge.changeTarget(g, newUsed)

    g3.changeType(oldEdge.user, g.getConcreteNode(oldEdge.user).styp,
      oldEdge.used, newUsed)
  }

  implicit val defaulVerbosity : PuckLog.Verbosity =
    (PuckLog.GraphTransfoRules, PuckLog.Debug)
  implicit def logVerbosity(lvl : PuckLog.Level) : PuckLog.Verbosity =
    (PuckLog.GraphTransfoRules, lvl)

  def redirectUsesAndPropagate(g  : DependencyGraph,
                               oldUse : DGUses, newUsed : NodeId,
                               policy : RedirectionPolicy,
                               propagateRedirection : Boolean = true,
                               keepOldUse : Boolean = false ) : LoggedTG = {
    val log0 = s"redirectUsesAndPropagate(_, oldUse = $oldUse, newUsed = $newUsed, $policy, " +
      s"propagate = $propagateRedirection, keepOldUse = $keepOldUse)"
    val log1 = s"\n$oldUse.exists = ${oldUse.existsIn(g)}"
    val lg1 = (g logComment log0) :++> log1

    if(oldUse.used == newUsed) lg1.toLoggedOr
    else if(oldUse.existsIn(g)){
      val lg2 = lg1 :++> s"\nredirecting ${showDG[DGEdge](g).show(oldUse)} target " +
        s"to ${showDG[NodeId](g).show(newUsed)} ($policy)"

      if (!propagateRedirection)
        lg2.map(redirectUses(_, oldUse, newUsed, keepOldUse)).toLoggedOr
      else {

        val oldUsedKind = g.kindType(oldUse.used)
        val ltg : LoggedTG = (lg2 :++> s"\nuse target is a $oldUsedKind").toLoggedOr

        val propagateTypeMember : DependencyGraph => LoggedTG =
          redirectTypeUsesOfTypeMemberUse(_, oldUse, newUsed, policy)

        val propagateType : DependencyGraph => LoggedOr[PuckError, (KeepOldTypeUse, DependencyGraph)] =
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
      lg1 :++> s"\nredirecting uses $oldUse target to $newUsed ($policy) : " +
        s"FAILURE !! ${oldUse.used}} is not used"
      lg1.toLoggedOr
    }
    else if(g.usersOf(oldUse.used).contains(oldUse.user) ||
      g.usersOf(newUsed).contains(oldUse.user))
      lg1.toLoggedOr[PuckError].error(new DGError("incoherent state !!!!!!!!!!!!"))
    else
      lg1.toLoggedOr[PuckError].error(new DGError(s"redirecting uses ${showDG[DGEdge](g).show(oldUse)} target to ${showDG[NodeId](g).show(newUsed)} ($policy)\n" +
        s"!!! nor the oldUsee or the newUsee is really used !!! "))

  }

  def redirectTypeUsesOfTypeMemberUse(lg0 : DependencyGraph,
                                      currentTypeMemberUse : DGUses,
                                      newTypeMemberUsed : NodeId,
                                      policy : RedirectionPolicy,
                                      propagateRedirection : Boolean = true) : LoggedTG = {

    val g = lg0.value
    val log = s"redirecting Type uses of typeMember use ${showDG[DGEdge](g).shows(currentTypeMemberUse)}" +
      s" (new typeMember used is ${showDG[NodeId](g).shows(newTypeMemberUsed)}) "
    val lg = lg0 comment log

    val typeUses = g.typeUsesOf(currentTypeMemberUse)
    if(typeUses.isEmpty)
      (lg :++> "\nno primary uses to redirect") map (_.right)
    else{
      val lg1 =
        lg :++> "uses to redirect:%s".format(typeUses.mkString("\n\t", "\n\t","\n"))

      foldLog(typeUses, lg1){(lg0, typeUse) =>

        val keepOldUse = g.typeMemberUsesOf(typeUse).tail.nonEmpty //is empty if typeUses had only one side use

        val redirect : LoggedG => Try[(DGUses, DependencyGraph)] =
          if(typeUse.selfUse)
            lg => ??? //redirectThisTypeUse(g, typeUses.used, newTypeMemberUsed)
          else {
            lg =>
              findNewTypeUsed(g, typeUse.used, newTypeMemberUsed, policy).flatMap {
                case (g, newTypeUsed) =>
                  redirectUsesAndPropagate(g, typeUse, newTypeUsed,
                    policy, propagateRedirection, keepOldUse).map{_.map((Uses(typeUse.user, newTypeUsed),_))}
              }


          }
        val g1 = lg0.map(_.removeUsesDependency(typeUse, currentTypeMemberUse))

        for(
          eg <- redirect(g1);
          (newTypeUse, g2) = eg
        ) yield
          g2.addUsesDependency(newTypeUse, Uses(currentTypeMemberUse.user, newTypeMemberUsed))

      }
    }
  }


  def findNewTypeUsed
  ( lg : LoggedG,
    currentTypeUsed : NodeId,
    newTypeMemberUsed : NodeId,
    policy : RedirectionPolicy
    ) : Logged[(DependencyGraph, NodeId)] = {
    val g = lg.value
    val log = s"searching new Type used ($policy) : current type used is " +
      s"${showDG[NodeId](g).shows(currentTypeUsed)}, new typeMember used : ${showDG[NodeId](g).shows(newTypeMemberUsed)}"
    val lg1= lg comment log

    val newPrimaryUsed =
      policy match {
        case NotAnAbstraction => g.container(newTypeMemberUsed).get
        case absPolicy : AbstractionPolicy =>
          g.abstractions(currentTypeUsed).find {
            case (node, `absPolicy`) => g.contains_*(node, newTypeMemberUsed)
            case _ => false
          } match {
            case Some((n, _)) => n
            case None =>
              val abstractKinds =
                g.getNode(currentTypeUsed).kind.
                  abstractKinds(absPolicy)

              g.nodesId.find{ node =>
                g.contains_*(node, newTypeMemberUsed) && {
                  abstractKinds.contains(g.getNode(node).kind)
                }

              } match {
                case Some(n) =>
                  g.logger.writeln(n + " found as type used")
                  n
                case None =>
                  throw new RedirectionError("no correct primary abstraction found !")
              }
          }
      }
    (lg1 :++> s"new type to use found : ${showDG[NodeId](g).shows(newPrimaryUsed)}\n").map{
      (_, newPrimaryUsed)
    }

  }



  type TypeMemberUses = List[DGUses]
  def redirectTypeMemberUsesOfTypeUse
    (lg : LoggedG,
     currentTypeUse: DGUses,
     newTypeUsed : NodeId,
     policy : RedirectionPolicy,
     tmu : TypeMemberUses) : LoggedTG = {

    val g = lg.value
    val log = s"redirecting typeMember uses of type use ${showDG[DGEdge](g).shows(currentTypeUse)} " +
      s"(new type used is  ${showDG[NodeId](g).shows(newTypeUsed)}) "

    val log1 = if (tmu.isEmpty) "no typeMember uses to redirect"
    else "uses to redirect:%s".format(tmu.mkString("\n\t", "\n\t", "\n")

    val lg1 = lg.map(_.comment(log)) :++> (log + "\n" + log1)



    foldLog(tmu, lg1) {
      case (lg0, typeMemberUse) =>
        val typeMember = typeMemberUse.used
        val someTypeMemberAbs =
          g.abstractions(typeMember).find { case (abs, _) => g.contains(newTypeUsed, abs) }

        someTypeMemberAbs match {
          case None =>
            val msg = s"While redirecting type use ${showDG[DGEdge](g).shows(currentTypeUse)} " +
              s"target to ${showDG[NodeId](g).shows(newTypeUsed)}\n" +
              s"no satisfying abstraction to redirect typeMember use ${showDG[DGEdge](g).shows(typeMemberUse)}"

            (lg0 :++> msg) map ( _ => -\/(new RedirectionError(msg)))

          case Some((typeMemberAbs, _)) =>
            redirectUsesAndPropagate(
              lg0.map(_.removeUsesDependency(currentTypeUse, typeMemberUse)),
              typeMemberUse, typeMemberAbs, policy).map {
              _.map {
                g2 =>
                  val newSide = DGEdge.UsesK(typeMemberUse.user, typeMemberAbs)
                  g2.addUsesDependency(Uses(currentTypeUse.user, newTypeUsed), newSide)
              }
            }
        }
    }

  }



  type KeepOldTypeUse = Boolean
  def redirectTypeMemberAndConstructorUsesOfTypeUse
  ( lg0 : LoggedG,
    currentTypeUse: DGUses,
    newTypeUsed : NodeId,
    policy : RedirectionPolicy
    ): Logged[Try[(KeepOldTypeUse, DependencyGraph)]] = {
    val g = lg0.value

    val log = s"redirecting typeMember AND CONSTRUCTOR uses of type use ${showDG[DGEdge](g).shows(currentTypeUse)} " +
      s"(new type used is  ${showDG[NodeId](g).shows(newTypeUsed)}) "
    val lg1 = lg0.map(_.comment(log)) :++> log


    val typeMemberAndTypeCtorUses = g.typeMemberUsesOf(currentTypeUse).toList
    import puck.util.Collections.SelectList
    typeMemberAndTypeCtorUses.select { e => g.kindType(g.getNode(e.target)) == TypeConstructor} match {
      case Some((typeCtorUse, typeMemberUses))
        if ! g.abstractions(typeCtorUse.used).exists {case (abs, _) => g.contains(newTypeUsed, abs)} =>
        redirectTypeMemberUsesOfTypeUse(lg1, currentTypeUse, newTypeUsed, policy, typeMemberUses).map(_.map((true, _)))
      case _ =>
        redirectTypeMemberUsesOfTypeUse(lg1, currentTypeUse, newTypeUsed, policy, typeMemberAndTypeCtorUses).map(_.map((false, _)))
    }
  }
}
