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

  def redirectUsesAndPropagate(g : DependencyGraph,
                               oldEdge : DGEdge, newUsed : NodeId,
                               policy : RedirectionPolicy,
                               propagateRedirection : Boolean = true,
                               keepOldUse : Boolean = false ) : Try[DependencyGraph] = {

    if(oldEdge.used == newUsed) \/-( g)
    else if(oldEdge.existsIn(g)){
      g.logger.writeln(s"redirecting ${showDG[DGEdge](g).show(oldEdge)} target " +
        s"to ${showDG[NodeId](g).show(newUsed)} ($policy)")

      if (!propagateRedirection)
        \/-(redirectUses(g, oldEdge, newUsed, keepOldUse))
      else {

        val oldUsedKind = g.kindType(oldEdge.used)
        g.logger.writeln(s"use target is a $oldUsedKind")
        val propagateTypeMember : DependencyGraph => Try[DependencyGraph] =
          redirectTypeUsesOfTypeMemberUse(_, oldEdge, newUsed, policy)

        val propagateType : DependencyGraph => Try[(KeepOldTypeUse, DependencyGraph)] =
          redirectTypeMemberAndConstructorUsesOfTypeUse(_, oldEdge, newUsed, policy)

        val tg = (oldUsedKind match {
          case TypeConstructor | TypeMember =>
            propagateTypeMember(g).map((false,_))
          case TypeDecl => propagateType(g)
          case TypeDeclAndTypeMember =>
            propagateTypeMember(g).flatMap(propagateType)
          case _ => \/-((false, g))

        }) map { case (keep, g2) =>
          redirectUses(g2, oldEdge, newUsed, keep || keepOldUse)
        }

        oldUsedKind match {
          case TypeConstructor =>
            val ctorId = oldEdge.used
            g.container(ctorId) match {
              case None =>
                -\/(new PuckError("constructor should have a container"))
              case Some(classId) => tg map { g0 =>
                g.usersOf(oldEdge.user).foldLeft(g0){ case (g1, userId) =>
                  g1.addUses(userId, ctorId).addUses(userId, classId)}
              }
            }
          case _ => tg
        }

      }
    }
    else if (g.uses(oldEdge.user, newUsed)) {
      //if m uses  both A and A.mi, the first uses dominate the second
      //if both are identified as violations and are in a wrongusers list
      //redirecting the one will redirect the other
      // when iterating on the wrongusers, the next call to redirectuses will arrive here
      g.logger.writeln(s"redirecting uses $oldEdge target to $newUsed ($policy) : " +
        s"FAILURE !! ${oldEdge.used}} is not used")
      \/-(g)
    }
    else if(g.usersOf(oldEdge.used).exists(_ == oldEdge.user) ||
      g.usersOf(newUsed).exists(_==oldEdge.user))
      -\/(new DGError("incoherent state !!!!!!!!!!!!"))
    else
      -\/(new DGError(s"redirecting uses ${showDG[DGEdge](g).show(oldEdge)} target to ${showDG[NodeId](g).show(newUsed)} ($policy)\n" +
        s"!!! nor the oldUsee or the newUsee is really used !!! "))

  }

  /*TODELETE
  def redirectThisTypeUse(g : GraphT, thisType : NodeId, movedId : NodeId): Try[(EdgeT, GraphT)] = {
    g.logger.writeln(s"redirecting This.Type use (${showDG[NodeId](g).shows(thisType)})")

    val typeNode = g.getConcreteNode(thisType)
    val movedNode = g.getConcreteNode(movedId)
    typeNode.kind match {
      case Class =>
        val newTypeUsed = findNewTypeUsed(g, thisType, movedId, Move)
        val (field, g2) = g.addConcreteNode(movedNode.name + "_delegate", Field, Some(new JavaNamedType(newTypeUsed)))
        val g3 = g2.addContains(thisType, field.id)
          .addUses(field.id, newTypeUsed)
          .addUses(movedId, field.id)
        \/-( (DGEdge.uses(field.id, newTypeUsed),g3))
      case _=>
        -\/(new PuckError(s"redirect type uses, expected class got ${typeNode.kind}"))
    }
  }
  */
  def redirectTypeUsesOfTypeMemberUse(g : DependencyGraph,
                                      currentTypeMemberUse : DGEdge,
                                      newTypeMemberUsed : NodeId,
                                      policy : RedirectionPolicy,
                                      propagateRedirection : Boolean = true) : Try[DependencyGraph] = {

    g.logger.writeln(s"redirecting Type uses of typeMember use ${showDG[DGEdge](g).shows(currentTypeMemberUse)}" +
      s" (new typeMember used is ${showDG[NodeId](g).shows(newTypeMemberUsed)}) ")

    val typeUses = g.typeUsesOf(currentTypeMemberUse)
    if(typeUses.isEmpty) {
      g.logger.writeln("no primary uses to redirect")
      \/-(g)
    }
    else{
      g.logger.writeln("uses to redirect:%s".format(typeUses.mkString("\n\t", "\n\t","\n")))

      traverse(typeUses, g){(g0, typeUse0) =>
        val typeUses = DGEdge.uses(typeUse0)

        val keepOldUse = g.typeMemberUsesOf(typeUse0).tail.nonEmpty //is empty if typeUses had only one side use

        val isThisTypeUse = typeUses.source == typeUses.target

        val redirect : DependencyGraph => Try[(DGEdge, DependencyGraph)] =
          if(isThisTypeUse)
            g => ??? //redirectThisTypeUse(g, typeUses.used, newTypeMemberUsed)
          else {
            g =>
              val newTypeUsed = findNewTypeUsed(g, typeUses.used, newTypeMemberUsed, policy)
              redirectUsesAndPropagate(g, typeUses,newTypeUsed,
                policy, propagateRedirection, keepOldUse).map{(DGEdge.uses(typeUses.user, newTypeUsed),_)}
          }
        val g1 = g0.removeUsesDependency(typeUses, currentTypeMemberUse)

        for(
          eg <- redirect(g1);
          (newPrimary, g2) = eg
        ) yield
        g2.addUsesDependency(newPrimary, (currentTypeMemberUse.user, newTypeMemberUsed))

      }
    }
  }


  def findNewTypeUsed(g : DependencyGraph,
                      currentTypeUsed : NodeId,
                      newTypeMemberUsed : NodeId,
                      policy : RedirectionPolicy) : NodeId = {

    g.logger.writeln(s"searching new Type used ($policy) : current type used is " +
      s"${showDG[NodeId](g).shows(currentTypeUsed)}, new typeMember used : ${showDG[NodeId](g).shows(newTypeMemberUsed)}" )

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
                  g.logger.writeln(n + " found as primary usee")
                  n
                case None =>
                  throw new RedirectionError("no correct primary abstraction found !")
              }
          }
      }
    g.logger.writeln(s"new type to use found : ${showDG[NodeId](g).shows(newPrimaryUsed)}")
    newPrimaryUsed
  }



  type TypeMemberUses = List[(NodeId, NodeId)]
  def redirectTypeMemberUsesOfTypeUse(g : DependencyGraph,
                                      currentTypeUse: DGEdge,
                                      newTypeUsed : NodeId,
                                      policy : RedirectionPolicy,
                                      tmu : TypeMemberUses) : Try[DependencyGraph] = {
    g.logger.writeln(s"redirecting typeMember uses of type use ${showDG[DGEdge](g).shows(currentTypeUse)} " +
      s"(new type used is  ${showDG[NodeId](g).shows(newTypeUsed)}) ")

    if (tmu.isEmpty) g.logger.writeln("no typeMember uses to redirect")
    else g.logger.writeln("uses to redirect:%s".format(tmu.mkString("\n\t", "\n\t", "\n")))

    traverse(tmu, g) {
      case (g0, side0) =>
        val side = DGEdge.uses(side0)
        val typeMember = side.used
        val someTypeMemberAbs =
          g.abstractions(typeMember).find { case (abs, _) => g.contains(newTypeUsed, abs) }

        someTypeMemberAbs match {
          case None =>
            val msg = s"While redirecting type use ${showDG[DGEdge](g).shows(currentTypeUse)} " +
              s"target to ${showDG[NodeId](g).shows(newTypeUsed)}\n" +
              s"no satisfying abstraction to redirect typeMember use ${showDG[DGEdge](g).shows(side)}"

            g.logger.writeln(msg)(PuckLog.Error)
            -\/(new RedirectionError(msg))

          case Some((typeMemberAbs, _)) =>
            redirectUsesAndPropagate(g0.removeUsesDependency(currentTypeUse, side),
              side, typeMemberAbs, policy).map {
              g2 =>
                val newSide = DGEdge.uses(side.user, typeMemberAbs)
                g2.addUsesDependency((currentTypeUse.user, newTypeUsed), newSide)
            }
        }
    }

  }



  type KeepOldTypeUse = Boolean
  def redirectTypeMemberAndConstructorUsesOfTypeUse(g : DependencyGraph,
                                                    currentTypeUse: DGEdge,
                                                    newTypeUsed : NodeId,
                                                    policy : RedirectionPolicy): Try[(KeepOldTypeUse, DependencyGraph)] = {
    val typeMemberAndTypeCtorUses = g.typeMemberUsesOf(currentTypeUse).toList
    import puck.util.Collections.SelectList
    typeMemberAndTypeCtorUses.select { case (_, target) => g.kindType(g.getNode(target)) == TypeConstructor} match {
      case Some((typeCtorUse, typeMemberUses))
        if ! g.abstractions(typeCtorUse._2).exists {case (abs, _) => g.contains(newTypeUsed, abs)} =>
        redirectTypeMemberUsesOfTypeUse(g, currentTypeUse, newTypeUsed, policy, typeMemberUses).map((true, _))
      case _ =>
        redirectTypeMemberUsesOfTypeUse(g, currentTypeUse, newTypeUsed, policy, typeMemberAndTypeCtorUses).map((false,_))
    }
  }
}
