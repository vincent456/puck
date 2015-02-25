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
trait TransformationRules {

  type NIdT = NodeId
  //type NT = NodeT
  type EdgeT = DGEdge
  type GraphT = DependencyGraph
  type STyp = TypeHolder

  implicit val defaulVerbosity : PuckLog.Verbosity =
    (PuckLog.GraphTransfoRules, PuckLog.Debug)
  import scala.language.implicitConversions
  implicit def logVerbosity(lvl : PuckLog.Level) : PuckLog.Verbosity =
    (PuckLog.GraphTransfoRules, lvl)



  def abstractionName(g: GraphT, implId: NIdT, abskind : NodeKind, policy : AbstractionPolicy) : String =
    g.getNode(implId).name + "_" + policy

  def createNode(g : GraphT, implId: NIdT, abskind : NodeKind, policy : AbstractionPolicy) : (NIdT, GraphT) = {
    val implTypHolder = g.getNode(implId).styp
    val (id, g1) = g.addNode(abstractionName(g, implId, abskind, policy), abskind, implTypHolder)
    //val g2 = implTypHolder.getTypeNodeIds.foldLeft(g1){(g0, tid) => g0.addUses(id, tid)}
    (id, g1.addAbstraction(implId, (id, policy)))
  }



  def createAbstraction(g : GraphT,
                        implId: NIdT,
                        abskind : NodeKind ,
                        policy : AbstractionPolicy) : Try[(NIdT, GraphT)] = {
    val (absId, g1) = createNode(g, implId, abskind, policy)

    Success((absId, policy match {
      case SupertypeAbstraction => g1.addUses(implId, absId)
      case DelegationAbstraction => g1.addUses(absId, implId)
    }))

  }

  def abstractionCreationPostTreatment(g : GraphT,
                                       implId : NIdT,
                                       absId : NIdT,
                                       policy : AbstractionPolicy) : GraphT = g


  def redirectUsesOf(g : GraphT,
                   oldEdge : EdgeT, newUsed : NIdT,
                   policy : RedirectionPolicy,
                   propagateRedirection : Boolean = true,
                   keepOldUse : Boolean = false ) : Try[(EdgeT, GraphT)] = {

    if(oldEdge.used == newUsed) Success((oldEdge, g))
    else if(oldEdge.exists(g)){
      g.logger.writeln(s"redirecting ${showDG[EdgeT](g).show(oldEdge)} target " +
        s"to ${showDG[NIdT](g).show(newUsed)} ($policy)")

      val newUse : EdgeT = DGEdge.uses(oldEdge.user, newUsed)

      val g2 =
        if(keepOldUse)
          newUse.create(g)
        else
          oldEdge.changeTarget(g, newUsed)


      val g3 = g.getNode(oldEdge.user).styp match {
        case NoType => g2
        case sTyp => g2.changeType(oldEdge.user, sTyp, oldEdge.used, newUsed)
      }

      val tryG4 = if(propagateRedirection) {
        redirectTypeUsesOf(g3, oldEdge, newUsed, policy).flatMap {
          redirectTypeMemberUse(_, oldEdge, newUsed, policy)
        }
      }
      else Success(g3)

      tryG4 map {(newUse, _)}
    }
    else if (g.uses(oldEdge.user, newUsed)) {
      //if m uses  both A and A.mi, the first uses dominate the second
      //if both are identified as violations and are in a wrongusers list
      //redirecting the one will redirect the other
      // when iterating on the wrongusers, the next call to redirectuses will arrive here
      g.logger.writeln("redirecting uses %s target to %s (%s) : FAILURE !! %s is not used".
        format(oldEdge, newUsed, policy, oldEdge.used))
      Success((DGEdge.uses(oldEdge.user, newUsed), g))
    }
    else if(g.users(oldEdge.used).exists(_ == oldEdge.user) ||
      g.users(newUsed).exists(_==oldEdge.user))
      Failure(new DGError("incoherent state !!!!!!!!!!!!")).toValidationNel
    else
      Failure(new DGError(s"redirecting uses ${showDG[DGEdge](g).show(oldEdge)} target to ${showDG[NodeId](g).show(newUsed)} ($policy)\n" +
        s"!!! nor the oldUsee or the newUsee is really used !!! ")).toValidationNel

  }

  def redirectThisTypeUse(g : GraphT, thisType : NIdT, movedId : NIdT): Try[(EdgeT, GraphT)]

  def redirectTypeUsesOf(g : GraphT,
                          currentSideUse : EdgeT,
                          newSideUsed : NIdT,
                          policy : RedirectionPolicy,
                          propagateRedirection : Boolean = true) : Try[GraphT] = {





    g.logger.writeln(s"redirecting Type uses of side use ${showDG[EdgeT](g).shows(currentSideUse)}" +
      s" (new side usee is ${showDG[NIdT](g).shows(newSideUsed)}) ")

    val primaryUses = g.typeUsesOf(currentSideUse)
    if(primaryUses.isEmpty) {
      g.logger.writeln("no primary uses to redirect")
      Success(g)
    }
    else{


      g.logger.writeln("uses to redirect:%s".format(primaryUses.mkString("\n\t", "\n\t","\n")))

      primaryUses.foldLeft[Try[GraphT]](Success(g)){
        case (tryG0, primary0) =>
          val primary = DGEdge.uses(primary0)

          val keepOldUse = g.typeMemberUsesOf(primary0).nonEmpty //is empty if primary had only one side use
          val isThisTypeUse = primary.source == primary.target && g.isTypeUse(primary)

          val tryG1 : Try[GraphT] =
            tryG0.map { _.removeUsesDependency(primary, currentSideUse)}


          val tryG2 : Try[(EdgeT, GraphT)]= tryG1.flatMap { g1 =>
            if(isThisTypeUse)
              redirectThisTypeUse(g1, primary.used, newSideUsed)
            else
              redirectUsesOf(g1, primary,
                findNewTypeUsed(g1, primary.used, newSideUsed, policy),
                policy, propagateRedirection, keepOldUse)
          }

          tryG2 map {
            case (newPrimary, g2) =>
              g2.addUsesDependency(newPrimary, (currentSideUse.user, newSideUsed))
          }

      }
    }
  }

  def findNewTypeUsed(g : GraphT,
                         currentPrimaryUsed : NIdT,
                         newSideUsed : NIdT,
                         policy : RedirectionPolicy) : NIdT = {

    g.logger.writeln(s"searching new Type used ($policy) : currentPrimaryUsee is " +
       s"${showDG[NIdT](g).shows(currentPrimaryUsed)}, new side usee ${showDG[NIdT](g).shows(newSideUsed)}" )

    val newPrimaryUsed =
      policy match {
        case Move => g.container(newSideUsed).get
        case absPolicy : AbstractionPolicy =>
          g.abstractions(currentPrimaryUsed).find {
            case (node, `absPolicy`) => g.contains_*(node, newSideUsed)
            case _ => false
          } match {
            case Some((n, _)) => n
            case None =>
              val abstractKinds =
                g.getNode(currentPrimaryUsed).kind.
                  abstractKinds(absPolicy)

              g.nodesId.find{node =>
                g.contains_*(node, newSideUsed) && {
                  abstractKinds.contains(g.getNode(node).kind)
                }

              } match {
                case Some(n) =>
                  g.logger.writeln(n + " found as primary usee")

                  n
                case None =>
                  val msg = "no correct primary abstraction found !"
                  g.logger.writeln(msg)(PuckLog.Error)
                  throw new RedirectionError(msg)
              }
          }
      }
    g.logger.writeln(s"new primary usee found : ${showDG[NIdT](g).shows(newPrimaryUsed)}")
    newPrimaryUsed
  }


  def redirectTypeMemberUse(g : GraphT,
                       currentPrimaryUse: EdgeT,
                       newPrimaryUsed : NIdT,
                       policy : RedirectionPolicy) : Try[GraphT] = {
    g.logger.writeln(s"redirecting side uses of primary use ${showDG[EdgeT](g).shows(currentPrimaryUse)} " +
      s"(new primary usee is  ${showDG[NIdT](g).shows(newPrimaryUsed)}) ")

    val sideUses = g.typeMemberUsesOf(currentPrimaryUse)
    if(sideUses.isEmpty){
      g.logger.writeln("no side uses to redirect")
      Success(g)
    }
    else{
      g.logger.writeln("uses to redirect:%s".format(sideUses.mkString("\n\t", "\n\t","\n")))

      sideUses.foldLeft(Success(g) : Try[GraphT]){
        case (tryG, side0) =>
          val side = DGEdge.uses(side0)
          g.abstractions(side.used).find {
            case (abs, _) => g.contains(newPrimaryUsed, abs)
            case _ => false
          } match {
            case None =>

              val msg = s"While redirecting primary uses ${showDG[EdgeT](g).shows(currentPrimaryUse)} " +
                s"target to ${showDG[NIdT](g).shows(newPrimaryUsed)}\n" +
                s"no satisfying abstraction to redirect side use ${showDG[EdgeT](g).shows(side)}"

              g.logger.writeln(msg)(PuckLog.Error)
              Failure(new RedirectionError(msg)).toValidationNel
            case Some( (new_side_usee, _) ) =>

              val tryG1 : Try[GraphT] =
                tryG.map(_.removeUsesDependency(currentPrimaryUse, side))

              val tryG2 : Try[(EdgeT, GraphT)] =
                tryG1.flatMap(redirectUsesOf(_, side, new_side_usee, policy))

              tryG2.map {
                case (newSide, g2) =>
                  g2.addUsesDependency((currentPrimaryUse.user, newPrimaryUsed), newSide)
              }

          }
      }
    }
  }


  def moveTo(g : GraphT, movedId : NIdT, newContainer : NIdT): Try[GraphT] = {
    g.container(movedId) match {
      case None => Failure(new RedirectionError("moved node has no container !!!")).toValidationNel
      case Some(oldContainer) =>
        g.logger.writeln(s"moving ${showDG[NIdT](g).shows(movedId)} " +
          s"from ${showDG[NIdT](g).shows(oldContainer)} " +
          s"to ${showDG[NIdT](g).shows(newContainer)}")

        val g2 = g.changeSource(DGEdge.contains(oldContainer, movedId), newContainer)
        g.users(movedId).foldLeft[Try[GraphT]](Success(g2)){
          case (tg0, userId) =>
            tg0 flatMap (redirectTypeUsesOf(_, DGEdge.uses(userId, movedId), movedId,
              Move, propagateRedirection = false))
        }
    }

  }

  def addHideFromRootException(g : GraphT, node : NIdT, friend : NIdT): GraphT =
    g.newGraph(nConstraints = g.constraints.addHideFromRootException(g, node, friend))
  /*def addHideFromRootException(node : NIdT, friend : NIdT): GraphT = {
    constraints.printConstraints(g, logger, (PuckLog.InGraph, PuckLog.Debug))
    val ng = newGraph(nConstraints = constraints.addHideFromRootException(g, node,friend))
    ng.printConstraints(ng, logger, (PuckLog.InGraph, PuckLog.Debug))
    ng
  }*/

  def findMergingCandidate(g : GraphT, nid : NIdT) : Option[NIdT] = None

  def findMergingCandidateIn(g : GraphT, id : NIdT, root : NIdT) : Option[NIdT] = None

  //TODO deep merge : merge also content need to refactor find merging candidate
  //(deep merge is now done in JavaNode for interface node only)
  def merge(g : GraphT, consumerId : NIdT, consumedId : NIdT) : Try[GraphT] = {
    g.logger.writeln(s"merging ${g.getNode(consumedId)} into ${g.getNode(consumerId)}" )

    val consumed = g.getNode(consumedId)
    val consumer = g.getNode(consumerId)
    val g1 = g.users(consumedId).foldLeft(g) {
     (g0, userId) =>
        g0.changeTarget(DGEdge.uses(userId, consumedId), consumerId)
          .changeType(userId, g.getNode(userId).styp, consumedId, consumerId)
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


    val dominated_dominant_seq = g.memberUses2typeUsesMap.toSeq


    val g5 : GraphT = dominated_dominant_seq.foldLeft(g4) {
      case (g0, (( `consumedId`, sideUseeId), primUses)) =>
        primUses.foldLeft(g0) { case (g00, pUse) =>
          g00.addUsesDependency(pUse, (consumerId, sideUseeId))
            .removeUsesDependency(pUse, (consumedId, sideUseeId))
        }
      case (g0, _) => g0
    }


    val g6 = g.typeUses2memberUsesMap.toSeq.foldLeft(g5) {
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
              _.changeType(consumedChildId, g0.getNode(consumedChildId).styp, consumedId, consumerId)
            }
          }
        }
    }

    /*consumed.content.foldLeft(g6) {
      case (g0, childId) =>
        findMergingCandidateIn(childId, consumedId) match {
          case Some()
        }
        //println("moving " + childId +" into " + consumerId)

        /*val g1 = g0.changeSource(AGEdge.contains(consumedId, childId), consumerId)
        dominated_dominant_seq.foldLeft(g1){
          case (g00, ((sideUser, `childId`), primUses)) =>
            primUses.foldLeft(g00){case (g000, (pUser, `consumedId`)) =>
                val nPuser = if(pUser == consumedId) consumerId else pUser
                g000.addUsesDependency((sideUser, childId), (nPuser, consumerId))
                    .removeUsesDependency((sideUser, childId), (pUser, consumedId))
            case (g000, _) => g000
            }
          case (g00, _) => g00
        }*/
    }*/

    g7 map { g =>
      g.removeContains(g.container(consumedId).get, consumedId)
        .removeNode(consumedId)
    }

  }
}
