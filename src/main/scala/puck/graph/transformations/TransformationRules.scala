package puck
package graph
package transformations

//import puck.graph.{RedirectionError, AGError, NoType, AGEdge}

import ShowDG._
import graph.constraints._
import util.PuckLog

import scalaz._
import scalaz.Validation.FlatMap._
/**
 * Created by lorilan on 25/01/15.
 */

trait MergeMatcher {
  val node : ConcreteNode
  def canBeMergedInto(other : ConcreteNode, graph : DependencyGraph): Boolean = {
    other.kind == node.kind && other.id != node.id
  }
}

trait TransformationRules {

  //type NT = NodeT
  type EdgeT = DGEdge
  type GraphT = DependencyGraph

  implicit val defaulVerbosity : PuckLog.Verbosity =
    (PuckLog.GraphTransfoRules, PuckLog.Debug)
  import scala.language.implicitConversions
  implicit def logVerbosity(lvl : PuckLog.Level) : PuckLog.Verbosity =
    (PuckLog.GraphTransfoRules, lvl)


  //TODO see if it can be rewritten using scalaz !
  def traverse[A, B, E](a: Iterable[A], b: B)(f: (B, A) => Validation[E, B]): Validation[E,B] =
    a.foldLeft[Validation[E,B]](Success(b)){case (b0, a0) => b0 flatMap (f(_, a0))}

  def abstractionName(g: GraphT, impl: ConcreteNode, abskind : NodeKind, policy : AbstractionPolicy) : String =
    impl.name + "_" + policy

  def createAbsNode(g : GraphT, impl: ConcreteNode, abskind : NodeKind, policy : AbstractionPolicy) : (ConcreteNode, GraphT) = {
    val implTypHolder = impl.styp
    val (n, g1) = g.addConcreteNode(abstractionName(g, impl, abskind, policy), abskind, implTypHolder)
    //val g2 = implTypHolder.getTypeNodeIds.foldLeft(g1){(g0, tid) => g0.addUses(id, tid)}
    (n, g1.addAbstraction(impl.id, (n.id, policy)))
  }

  def createAbstraction(g : GraphT,
                        impl: ConcreteNode,
                        abskind : NodeKind ,
                        policy : AbstractionPolicy) : Try[(ConcreteNode, GraphT)] = {
    val (abs, g1) = createAbsNode(g, impl, abskind, policy)

    Success((abs, policy match {
      case SupertypeAbstraction => g1.addUses(impl.id, abs.id)
      case DelegationAbstraction => g1.addUses(abs.id, impl.id)
    }))

  }

  def abstractionCreationPostTreatment(g : GraphT,
                                       implId : NodeId,
                                       absId : NodeId,
                                       policy : AbstractionPolicy) : GraphT = g


  def redirectUses(g : GraphT,
                   oldEdge : EdgeT, newUsed : NodeId,
                   keepOldUse : Boolean) : GraphT = {

    val g3 = if(keepOldUse) g.addUses(oldEdge.user, newUsed)
    else oldEdge.changeTarget(g, newUsed)

    g3.changeType(oldEdge.user, g.getConcreteNode(oldEdge.user).styp,
      oldEdge.used, newUsed)
  }
  
  def redirectUsesAndPropagate(g : GraphT,
                   oldEdge : EdgeT, newUsed : NodeId,
                   policy : RedirectionPolicy,
                   propagateRedirection : Boolean = true,
                   keepOldUse : Boolean = false ) : Try[GraphT] = {

    if(oldEdge.used == newUsed) Success( g)
    else if(oldEdge.existsIn(g)){
      g.logger.writeln(s"redirecting ${showDG[EdgeT](g).show(oldEdge)} target " +
        s"to ${showDG[NodeId](g).show(newUsed)} ($policy)")

        if (!propagateRedirection)
          Success(redirectUses(g, oldEdge, newUsed, keepOldUse))
        else {

          val oldUsedKind = g.getNode(oldEdge.used).kind

          (if (oldUsedKind.isTypeConstructor || oldUsedKind.isTypeMember)
              propagateTypeMemberUseRedirection(g, oldEdge, newUsed, policy)
            else Success(g)).flatMap { g2 =>
            if (oldUsedKind.isTypeDecl){
              redirectTypeMemberAndConstructorUses(g2, oldEdge, newUsed, policy).map{
                case ((keepTypeUse, g3)) =>
                  redirectUses(g3, oldEdge, newUsed, keepOldUse || keepTypeUse)
              }
            }
            else Success(redirectUses(g2, oldEdge, newUsed, keepOldUse))
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
      Success(g)
    }
    else if(g.users(oldEdge.used).exists(_ == oldEdge.user) ||
      g.users(newUsed).exists(_==oldEdge.user))
      Failure(new DGError("incoherent state !!!!!!!!!!!!")).toValidationNel
    else
      Failure(new DGError(s"redirecting uses ${showDG[DGEdge](g).show(oldEdge)} target to ${showDG[NodeId](g).show(newUsed)} ($policy)\n" +
        s"!!! nor the oldUsee or the newUsee is really used !!! ")).toValidationNel

  }

  def redirectThisTypeUse(g : GraphT, thisType : NodeId, movedId : NodeId): Try[(EdgeT, GraphT)]

  def propagateTypeMemberUseRedirection(g : GraphT,
                          currentTypeMemberUse : EdgeT,
                          newTypeMemberUsed : NodeId,
                          policy : RedirectionPolicy,
                          propagateRedirection : Boolean = true) : Try[GraphT] = {

    g.logger.writeln(s"redirecting Type uses of typeMember use ${showDG[EdgeT](g).shows(currentTypeMemberUse)}" +
      s" (new typeMember used is ${showDG[NodeId](g).shows(newTypeMemberUsed)}) ")

    val typeUses = g.typeUsesOf(currentTypeMemberUse)
    if(typeUses.isEmpty) {
      g.logger.writeln("no primary uses to redirect")
      Success(g)
    }
    else{
      g.logger.writeln("uses to redirect:%s".format(typeUses.mkString("\n\t", "\n\t","\n")))

      traverse(typeUses, g){(g0, typeUse) =>
        val primary = DGEdge.uses(typeUse)

        val keepOldUse = g.typeMemberUsesOf(typeUse).tail.nonEmpty //is empty if primary had only one side use

        val isThisTypeUse = primary.source == primary.target && g.isTypeUse(primary)

          val redirect : GraphT => Try[(EdgeT, GraphT)] =
            if(isThisTypeUse)
              g => redirectThisTypeUse(g, primary.used, newTypeMemberUsed)
            else {
              g =>
                val newTypeUsed = findNewTypeUsed(g, primary.used, newTypeMemberUsed, policy)
                redirectUsesAndPropagate(g, primary,newTypeUsed,
                  policy, propagateRedirection, keepOldUse).map{(DGEdge.uses(primary.user, newTypeUsed),_)}
            }
          val g1 = g0.removeUsesDependency(primary, currentTypeMemberUse)

          for(
            eg <- redirect(g1);
            (newPrimary, g2) = eg
          ) yield
            g2.addUsesDependency(newPrimary, (currentTypeMemberUse.user, newTypeMemberUsed))

      }
    }
  }

  def findNewTypeUsed(g : GraphT,
                         currentTypeUsed : NodeId,
                         newTypeMemberUsed : NodeId,
                         policy : RedirectionPolicy) : NodeId = {

    g.logger.writeln(s"searching new Type used ($policy) : current type used is " +
       s"${showDG[NodeId](g).shows(currentTypeUsed)}, new typeMember used : ${showDG[NodeId](g).shows(newTypeMemberUsed)}" )

    val newPrimaryUsed =
      policy match {
        case Move => g.container(newTypeMemberUsed).get
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

              g.nodesId.find{node =>
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
  def redirectTypeMemberUses(g : GraphT,
                             currentTypeUse: EdgeT,
                             newTypeUsed : NodeId,
                             policy : RedirectionPolicy,
                             tmu : TypeMemberUses) : Try[GraphT] = {
    g.logger.writeln(s"redirecting typeMember uses of type use ${showDG[EdgeT](g).shows(currentTypeUse)} " +
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
              val msg = s"While redirecting type use ${showDG[EdgeT](g).shows(currentTypeUse)} " +
                s"target to ${showDG[NodeId](g).shows(newTypeUsed)}\n" +
                s"no satisfying abstraction to redirect typeMember use ${showDG[EdgeT](g).shows(side)}"

              g.logger.writeln(msg)(PuckLog.Error)
              Failure(new RedirectionError(msg)).toValidationNel

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
  def redirectTypeMemberAndConstructorUses(g : GraphT,
                                           currentTypeUse: EdgeT,
                                           newTypeUsed : NodeId,
                                           policy : RedirectionPolicy): Try[(KeepOldTypeUse, GraphT)] = {
    val typeMemberAndTypeCtorUses = g.typeMemberUsesOf(currentTypeUse).toList
    import puck.util.Collections.SelectList
    typeMemberAndTypeCtorUses.select{case (_, target) => g.getNode(target).kind.isTypeConstructor} match {
      case Some((typeCtorUse, typeMemberUses))
        if ! g.abstractions(typeCtorUse._2).exists {case (abs, _) => g.contains(newTypeUsed, abs)} =>
        redirectTypeMemberUses(g, currentTypeUse, newTypeUsed, policy, typeMemberUses).map((true, _))
      case _ =>
        redirectTypeMemberUses(g, currentTypeUse, newTypeUsed, policy, typeMemberAndTypeCtorUses).map((false,_))
    }
  }

  def moveTo(g : GraphT, movedId : NodeId, newContainer : NodeId): Try[GraphT] = {
    g.container(movedId) match {
      case None =>
        Failure(new RedirectionError("moved node has no container !!!")).toValidationNel
      case Some(oldContainer) =>
        g.logger.writeln(s"moving ${showDG[NodeId](g).shows(movedId)} " +
          s"from ${showDG[NodeId](g).shows(oldContainer)} " +
          s"to ${showDG[NodeId](g).shows(newContainer)}")

        val g2 = g.changeSource(DGEdge.contains(oldContainer, movedId), newContainer)
        g.users(movedId).foldLeft[Try[GraphT]](Success(g2)){
          case (tg0, userId) =>
            tg0 flatMap (propagateTypeMemberUseRedirection(_, DGEdge.uses(userId, movedId), movedId,
              Move, propagateRedirection = false))
        }
    }

  }

  def addHideFromRootException(g : GraphT, node : NodeId, friend : NodeId): GraphT =
    g.newGraph(nConstraints = g.constraints.addHideFromRootException(g, node, friend))
  /*def addHideFromRootException(node : NIdT, friend : NIdT): GraphT = {
    constraints.printConstraints(g, logger, (PuckLog.InGraph, PuckLog.Debug))
    val ng = newGraph(nConstraints = constraints.addHideFromRootException(g, node,friend))
    ng.printConstraints(ng, logger, (PuckLog.InGraph, PuckLog.Debug))
    ng
  }*/

  implicit def mergeMatcher(n : ConcreteNode): MergeMatcher

  def findMergingCandidate(g : GraphT, nid : ConcreteNode) : Option[ConcreteNode] = None

  def findMergingCandidateIn(g : GraphT, methodId : NodeId,  interfaceId : NodeId): Option[NodeId] =
    findMergingCandidateIn(g, g.getConcreteNode(methodId), g.getConcreteNode(interfaceId))

  def findMergingCandidateIn(g : GraphT, method : ConcreteNode, interface : ConcreteNode) : Option[NodeId]

  def merge(g : GraphT, consumerId : NodeId, consumedId : NodeId) : Try[GraphT] = {
    g.logger.writeln(s"merging ${g.getNode(consumedId)} into ${g.getNode(consumerId)}" )

     val g1 = g.users(consumedId).foldLeft(g) {
     (g0, userId) =>
        g0.changeTarget(DGEdge.uses(userId, consumedId), consumerId)
          .changeType(userId, g.getConcreteNode(userId).styp, consumedId, consumerId)
    }

    val g2 = g.usedBy(consumedId).foldLeft(g1) {
      (g0, usedId) => g0.changeSource(DGEdge.uses(consumedId, usedId), consumerId)
    }

    val g3 = g.directSuperTypes(consumedId).foldLeft(g2) {
      (g0, stId) =>
        if(stId != consumerId) g0.changeSource(DGEdge.isa(consumedId, stId), consumerId)
        else g0.removeIsa(consumedId, stId)
    }

    val g4 = g.directSubTypes(consumedId).foldLeft(g3) {
      (g0, stId) =>
        if(stId != consumerId) g0.changeTarget(DGEdge.isa(stId, consumedId), consumerId)
        else g0.removeIsa(stId, consumedId)
    }

    /*(consumerId, key) is a primary uses and sidesUses(key) are the corresponding side uses */
    //val sideUses = new UsesDependencyMap(consumerId, Dominant())

    /*(other, key) is a side uses and primaryUses(key) is the corresponding primary uses */
    //val primaryUses = new UsesDependencyMap(consumerId, Dominated())


    val dominated_dominant_seq = g.typeMemberUses2typeUsesMap.toSeq


    val g5 : GraphT = dominated_dominant_seq.foldLeft(g4) {
      case (g0, (( `consumedId`, sideUseeId), primUses)) =>
        primUses.foldLeft(g0) { case (g00, pUse) =>
          g00.addUsesDependency(pUse, (consumerId, sideUseeId))
            .removeUsesDependency(pUse, (consumedId, sideUseeId))
        }
      case (g0, _) => g0
    }


    val g6 = g.typeUses2typeMemberUsesMap.toSeq.foldLeft(g5) {
      case (g0, (( `consumedId`, primeUseeId), sidUses)) =>
        sidUses.foldLeft(g0) { case (g00, sUse) =>
          g00.addUsesDependency((consumerId, primeUseeId), sUse)
            .removeUsesDependency((consumedId, primeUseeId), sUse)
        }
      case (g0, _) => g0
    }



    val g7 = g.content(consumedId).foldLeft(Success(g6): Try[GraphT]) {
      (tg0, consumedChildId) =>
        tg0 flatMap { g0 =>
          findMergingCandidateIn(g0, consumedChildId, consumerId) match {
            case Some(consumerChildId) => merge(g0, consumerChildId, consumedChildId)
            case None => moveTo(g0, consumedChildId, consumerId) map {
              _.changeType(consumedChildId, g0.getConcreteNode(consumedChildId).styp, consumedId, consumerId)
            }
          }
        }
    }

    g7 map { g =>
      g.removeContains(g.container(consumedId).get, consumedId)
        .removeConcreteNode(consumedId)
    }

  }
}
