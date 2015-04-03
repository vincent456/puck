package puck
package graph
package transformations

//import puck.graph.{RedirectionError, AGError, NoType, AGEdge}
import puck.graph.transformations.rules.Move
import ShowDG._
import graph.constraints._
import util.PuckLog

import scalaz.{-\/, \/-, \/}

object TransformationRules{
  //see if it can be rewritten using scalaz !
  //it cannot Validation is not a monad ! if we use either instead ...
  def traverse[A, B, E](a: Iterable[A], b: B)(f: (B, A) => E \/ B): E \/ B =
    a.foldLeft[E\/B](\/-(b)){case (b0, a0) => b0 flatMap (f(_, a0))}

  def redirectUses(g : DependencyGraph,
                   oldEdge : DGEdge, newUsed : NodeId,
                   keepOldUse : Boolean) : DependencyGraph = {

    val g3 = if(keepOldUse) g.addUses(oldEdge.user, newUsed)
    else oldEdge.changeTarget(g, newUsed)

    g3.changeType(oldEdge.user, g.getConcreteNode(oldEdge.user).styp,
      oldEdge.used, newUsed)
  }

}

import TransformationRules._

trait TransformationRules {

  //type NT = NodeT

  def moveTypeDecl(g : DependencyGraph, movedId : NodeId, newContainer : NodeId): Try[DependencyGraph] =
    Move.moveTypeDecl(g, movedId, newContainer)

  def moveTypeMember(g : DependencyGraph,
                     typeMemberMoved : NodeId, newContainer : NodeId,
                     createVarStrategy: Move.CreateVarStrategy = Move.CreateParameter) =
    Move.moveTypeMember(g, typeMemberMoved, newContainer, createVarStrategy)


  implicit val defaulVerbosity : PuckLog.Verbosity =
    (PuckLog.GraphTransfoRules, PuckLog.Debug)
  import scala.language.implicitConversions
  implicit def logVerbosity(lvl : PuckLog.Level) : PuckLog.Verbosity =
    (PuckLog.GraphTransfoRules, lvl)


  def absIntroPredicate(graph : DependencyGraph,
                        impl : DGNode,
                        absPolicy : AbstractionPolicy,
                        absKind : NodeKind) : NodePredicateT = absPolicy match {
    case SupertypeAbstraction =>
      (graph, potentialHost) => !graph.interloperOf(impl.id, potentialHost.id)

    case DelegationAbstraction =>
      (graph, potentialHost) => !graph.interloperOf(potentialHost.id, impl.id)
  }



  def abstractionName(g: DependencyGraph, impl: ConcreteNode, abskind : NodeKind, policy : AbstractionPolicy) : String =
    impl.name + "_" + policy

  def createAbsNode(g : DependencyGraph, impl: ConcreteNode, abskind : NodeKind, policy : AbstractionPolicy) : (ConcreteNode, DependencyGraph) = {
    val implTypHolder = impl.styp
    val (n, g1) = g.addConcreteNode(abstractionName(g, impl, abskind, policy), abskind, implTypHolder)
    //val g2 = implTypHolder.getTypeNodeIds.foldLeft(g1){(g0, tid) => g0.addUses(id, tid)}
    (n, g1.addAbstraction(impl.id, (n.id, policy)))
  }

  def createAbstraction(g : DependencyGraph,
                        impl: ConcreteNode,
                        abskind : NodeKind ,
                        policy : AbstractionPolicy) : Try[(ConcreteNode, DependencyGraph)] = {
    val (abs, g1) = createAbsNode(g, impl, abskind, policy)

    \/-((abs, policy match {
      case SupertypeAbstraction => g1.addUses(impl.id, abs.id)
      case DelegationAbstraction => g1.addUses(abs.id, impl.id)
    }))

  }

  def abstractionCreationPostTreatment(g : DependencyGraph,
                                       implId : NodeId,
                                       absId : NodeId,
                                       policy : AbstractionPolicy) : DependencyGraph = g



  
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

          (oldUsedKind match {
            case TypeConstructor | TypeMember =>
              propagateTypeMember(g).map((false,_))
            case TypeDecl => propagateType(g)
            case TypeDeclMember =>
              propagateTypeMember(g).flatMap(propagateType)
            case _ => \/-((false, g))

           }) map { case (keep, g2) =>
            redirectUses(g2, oldEdge, newUsed, keep || keepOldUse)
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
    else if(g.users(oldEdge.used).exists(_ == oldEdge.user) ||
      g.users(newUsed).exists(_==oldEdge.user))
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
        //case Move => g.container(newTypeMemberUsed).get
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



  def addHideFromRootException(g : DependencyGraph, node : NodeId, friend : NodeId): DependencyGraph =
    g.newGraph(nConstraints = g.constraints.addHideFromRootException(g, node, friend))
  /*def addHideFromRootException(node : NIdT, friend : NIdT): GraphT = {
    constraints.printConstraints(g, logger, (PuckLog.InGraph, PuckLog.Debug))
    val ng = newGraph(nConstraints = constraints.addHideFromRootException(g, node,friend))
    ng.printConstraints(ng, logger, (PuckLog.InGraph, PuckLog.Debug))
    ng
  }*/


  def mergeMatcherInstances : MergeMatcherInstances

  implicit def mergeMatcher(n : ConcreteNode): MergeMatcher =
    mergeMatcherInstances.semanticMergeMatcher(n)

  def findMergingCandidate(g : DependencyGraph, nid : ConcreteNode) : Option[ConcreteNode] = None

  def findMergingCandidateIn(g : DependencyGraph, methodId : NodeId,  interfaceId : NodeId): Option[NodeId] =
    findMergingCandidateIn(g, g.getConcreteNode(methodId), g.getConcreteNode(interfaceId))

  def findMergingCandidateIn(g : DependencyGraph, method : ConcreteNode, interface : ConcreteNode) : Option[NodeId]

  def merge(g : DependencyGraph, consumerId : NodeId, consumedId : NodeId) : Try[DependencyGraph] = {
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


    val g5 : DependencyGraph = dominated_dominant_seq.foldLeft(g4) {
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

    val g7 = g.content(consumedId).foldLeft(\/-(g6): Try[DependencyGraph]) {
      (tg0, consumedChildId) =>
        tg0 flatMap { g0 =>
          findMergingCandidateIn(g0, consumedChildId, consumerId) match {
            case Some(consumerChildId) => merge(g0, consumerChildId, consumedChildId)
            case None =>
              g0.kindType(consumedChildId) match {
                case TypeDecl =>  Move.moveTypeDecl(g0, consumedChildId, consumerId)
                case TypeMember =>
                  Move.moveTypeMember(g0, consumedChildId, consumerId, ???) map {
                    _.changeType(consumedChildId, g0.getConcreteNode(consumedChildId).styp, consumedId, consumerId)
                  }
                case _ => ???
              }
          }
        }
    }

    g7 map { g =>
      g.removeContains(g.container(consumedId).get, consumedId)
        .removeConcreteNode(consumedId)
    }

  }

  def removeConcreteNode(graph : DependencyGraph,
                         n : ConcreteNode) :Try[DependencyGraph] =
    
    for{
      g1 <- traverse(graph.content(n.id).map(graph.getConcreteNode), graph)(removeConcreteNode)
      g2 <- 
        if(g1.users(n.id).nonEmpty)
          -\/(new PuckError("Cannot remove a used node"))
        else {
          val g00 = g1.removeContains(g1.container(n.id).get, n.id)
          val g01 = graph.directSuperTypes(n.id).foldLeft(g00){
            (g, supId) => g.removeIsa(n.id, supId)
          }
          val g02 = graph.usedBy(n.id).foldLeft(g01){
            (g, usedId) => g.removeUses(n.id, usedId)
          }
          \/-(g02.removeConcreteNode(n.id))
        }  
      
    } yield g2
  
}
