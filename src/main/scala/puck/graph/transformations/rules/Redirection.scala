package puck.graph.transformations.rules

import puck.PuckError
import puck.graph.ShowDG._
import puck.graph._
import puck.graph.constraints.{NotAnAbstraction, AbstractionPolicy, RedirectionPolicy}
import puck.util.PuckLog
import puck.util.Collections.traverse
import scalaz._

object Redirection {

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

  def redirectUsesAndPropagate(g0 : DependencyGraph,
                               oldUse : DGUses, newUsed : NodeId,
                               policy : RedirectionPolicy,
                               propagateRedirection : Boolean = true,
                               keepOldUse : Boolean = false ) : Try[DependencyGraph] = {
    val log = s"redirectUsesAndPropagate(_, oldUse = $oldUse, newUsed = $newUsed, $policy, " +
      s"propagate = $propagateRedirection, keepOldUse = $keepOldUse)"
    val g = g0.comment(log)
    g.logger.writeln(log)
    g.logger.writeln(s"$oldUse.exists = ${oldUse.existsIn(g)}")
    if(oldUse.used == newUsed) \/-( g)
    else if(oldUse.existsIn(g)){
      g.logger.writeln(s"redirecting ${showDG[DGEdge](g).show(oldUse)} target " +
        s"to ${showDG[NodeId](g).show(newUsed)} ($policy)")

      if (!propagateRedirection)
        \/-(redirectUses(g, oldUse, newUsed, keepOldUse))
      else {

        val oldUsedKind = g.kindType(oldUse.used)
        g.logger.writeln(s"use target is a $oldUsedKind")
        val propagateTypeMember : DependencyGraph => Try[DependencyGraph] =
          redirectTypeUsesOfTypeMemberUse(_, oldUse, newUsed, policy)

        val propagateType : DependencyGraph => Try[(KeepOldTypeUse, DependencyGraph)] =
          redirectTypeMemberAndConstructorUsesOfTypeUse(_, oldUse, newUsed, policy)

        val tg = (oldUsedKind match {
          case TypeConstructor | TypeMember =>
            propagateTypeMember(g).map((false,_))
          case TypeDecl => propagateType(g)
          case TypeDeclAndTypeMember =>
            propagateTypeMember(g).flatMap(propagateType)
          case _ => \/-((false, g))

        }) map { case (keep, g2) =>
          redirectUses(g2, oldUse, newUsed, keep || keepOldUse)
        }

        oldUsedKind match {
          case TypeConstructor =>
            val ctorId = oldUse.used
            g.container(ctorId) match {
              case None =>
                -\/(new PuckError("constructor should have a container"))
              case Some(classId) => tg map { g0 =>
                g.usersOf(oldUse.user).foldLeft(g0){ case (g1, userId) =>
                  g1.addUses(userId, ctorId).addUses(userId, classId)}
              }
            }
          case _ => tg
        }

      }
    }
    else if (g.uses(oldUse.user, newUsed)) {
      //if m uses  both A and A.mi, the first uses dominate the second
      //if both are identified as violations and are in a wrongusers list
      //redirecting the one will redirect the other
      // when iterating on the wrongusers, the next call to redirectuses will arrive here
      g.logger.writeln(s"redirecting uses $oldUse target to $newUsed ($policy) : " +
        s"FAILURE !! ${oldUse.used}} is not used")
      \/-(g)
    }
    else if(g.usersOf(oldUse.used).contains(oldUse.user) ||
      g.usersOf(newUsed).contains(oldUse.user))
      -\/(new DGError("incoherent state !!!!!!!!!!!!"))
    else
      -\/(new DGError(s"redirecting uses ${showDG[DGEdge](g).show(oldUse)} target to ${showDG[NodeId](g).show(newUsed)} ($policy)\n" +
        s"!!! nor the oldUsee or the newUsee is really used !!! "))

  }

  def redirectTypeUsesOfTypeMemberUse(g0 : DependencyGraph,
                                      currentTypeMemberUse : DGUses,
                                      newTypeMemberUsed : NodeId,
                                      policy : RedirectionPolicy,
                                      propagateRedirection : Boolean = true) : Try[DependencyGraph] = {

    val log = s"redirecting Type uses of typeMember use ${showDG[DGEdge](g0).shows(currentTypeMemberUse)}" +
      s" (new typeMember used is ${showDG[NodeId](g0).shows(newTypeMemberUsed)}) "
    val g= g0.comment(log)
    g.logger.writeln(log)

    val typeUses = g.typeUsesOf(currentTypeMemberUse)
    if(typeUses.isEmpty) {
      g.logger.writeln("no primary uses to redirect")
      \/-(g)
    }
    else{
      g.logger.writeln("uses to redirect:%s".format(typeUses.mkString("\n\t", "\n\t","\n")))

      traverse(typeUses, g){(g0, typeUse) =>

        val keepOldUse = g.typeMemberUsesOf(typeUse).tail.nonEmpty //is empty if typeUses had only one side use

        val redirect : DependencyGraph => Try[(DGUses, DependencyGraph)] =
          if(typeUse.selfUse)
            g => ??? //redirectThisTypeUse(g, typeUses.used, newTypeMemberUsed)
          else {
            g =>
              val newTypeUsed = findNewTypeUsed(g, typeUse.used, newTypeMemberUsed, policy)
              redirectUsesAndPropagate(g, typeUse, newTypeUsed,
                policy, propagateRedirection, keepOldUse).map{(Uses(typeUse.user, newTypeUsed),_)}
          }
        val g1 = g0.removeUsesDependency(typeUse, currentTypeMemberUse)

        for(
          eg <- redirect(g1);
          (newTypeUse, g2) = eg
        ) yield
          g2.addUsesDependency(newTypeUse, Uses(currentTypeMemberUse.user, newTypeMemberUsed))

      }
    }
  }


  def findNewTypeUsed(g0 : DependencyGraph,
                      currentTypeUsed : NodeId,
                      newTypeMemberUsed : NodeId,
                      policy : RedirectionPolicy) : NodeId = {

    val log = s"searching new Type used ($policy) : current type used is " +
      s"${showDG[NodeId](g0).shows(currentTypeUsed)}, new typeMember used : ${showDG[NodeId](g0).shows(newTypeMemberUsed)}"
    val g= g0.comment(log)
    g.logger.writeln(log)

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
    g.logger.writeln(s"new type to use found : ${showDG[NodeId](g).shows(newPrimaryUsed)}")
    newPrimaryUsed
  }



  type TypeMemberUses = List[DGUses]
  def redirectTypeMemberUsesOfTypeUse(g0 : DependencyGraph,
                                      currentTypeUse: DGUses,
                                      newTypeUsed : NodeId,
                                      policy : RedirectionPolicy,
                                      tmu : TypeMemberUses) : Try[DependencyGraph] = {

    val log = s"redirecting typeMember uses of type use ${showDG[DGEdge](g0).shows(currentTypeUse)} " +
      s"(new type used is  ${showDG[NodeId](g0).shows(newTypeUsed)}) "
    val g= g0.comment(log)
    g.logger.writeln(log)

    if (tmu.isEmpty) g.logger.writeln("no typeMember uses to redirect")
    else g.logger.writeln("uses to redirect:%s".format(tmu.mkString("\n\t", "\n\t", "\n")))

    traverse(tmu, g) {
      case (g0, typeMemberUse) =>
        val typeMember = typeMemberUse.used
        val someTypeMemberAbs =
          g.abstractions(typeMember).find { case (abs, _) => g.contains(newTypeUsed, abs) }

        someTypeMemberAbs match {
          case None =>
            val msg = s"While redirecting type use ${showDG[DGEdge](g).shows(currentTypeUse)} " +
              s"target to ${showDG[NodeId](g).shows(newTypeUsed)}\n" +
              s"no satisfying abstraction to redirect typeMember use ${showDG[DGEdge](g).shows(typeMemberUse)}"

            g.logger.writeln(msg)(PuckLog.Error)
            -\/(new RedirectionError(msg))

          case Some((typeMemberAbs, _)) =>
            redirectUsesAndPropagate(g0.removeUsesDependency(currentTypeUse, typeMemberUse),
              typeMemberUse, typeMemberAbs, policy).map {
              g2 =>
                val newSide = DGEdge.UsesK(typeMemberUse.user, typeMemberAbs)
                g2.addUsesDependency(Uses(currentTypeUse.user, newTypeUsed), newSide)
            }
        }
    }

  }



  type KeepOldTypeUse = Boolean
  def redirectTypeMemberAndConstructorUsesOfTypeUse(g0 : DependencyGraph,
                                                    currentTypeUse: DGUses,
                                                    newTypeUsed : NodeId,
                                                    policy : RedirectionPolicy): Try[(KeepOldTypeUse, DependencyGraph)] = {
    val log = s"redirecting typeMember AND CONSTRUCTOR uses of type use ${showDG[DGEdge](g0).shows(currentTypeUse)} " +
      s"(new type used is  ${showDG[NodeId](g0).shows(newTypeUsed)}) "
    val g= g0.comment(log)
    val typeMemberAndTypeCtorUses = g.typeMemberUsesOf(currentTypeUse).toList
    import puck.util.Collections.SelectList
    typeMemberAndTypeCtorUses.select { e => g.kindType(g.getNode(e.target)) == TypeConstructor} match {
      case Some((typeCtorUse, typeMemberUses))
        if ! g.abstractions(typeCtorUse.used).exists {case (abs, _) => g.contains(newTypeUsed, abs)} =>
        redirectTypeMemberUsesOfTypeUse(g, currentTypeUse, newTypeUsed, policy, typeMemberUses).map((true, _))
      case _ =>
        redirectTypeMemberUsesOfTypeUse(g, currentTypeUse, newTypeUsed, policy, typeMemberAndTypeCtorUses).map((false,_))
    }
  }
}
