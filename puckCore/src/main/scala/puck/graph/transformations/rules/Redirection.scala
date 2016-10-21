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

import puck.PuckError
import puck.graph._
import puck.util.LoggedEither
import LoggedEither._

import scalaz.std.list._
import scalaz.std.set._
import ShowDG._
import puck.graph.comparison.Mapping

object Redirection {

  def redirectUsesAndPropagate
  (graph : DependencyGraph,
   oldUse : NodeIdP,
   newUsed : Abstraction
  ): LoggedTG = {

    val g = graph.comment(s"Redirection.redirectUsesAndPropagate(g, ${(graph,oldUse).shows}, ${(graph, newUsed).shows})")

    val oldUsed = g.getConcreteNode(oldUse.used)
    val oldKindType = oldUsed.kind.kindType
    val newKindType = newUsed.kindType(g)

    val ltg : LoggedTG = (oldKindType, newKindType) match {
      case (TypeDecl, TypeDecl) =>
        val AccessAbstraction(newUsedId, pol) = newUsed
        redirectInstanceUsesTowardAbstractionInAnotherTypeAndPropagate(g, oldUse, newUsedId, pol)

      case (InstanceValue, InstanceValue) =>
        if(oldUsed.kind.isWritable)
          redirectFieldTowardGetterSetter(g, oldUse, newUsed)
        else {
          redirectInstanceUsesTowardAbstractionInAnotherTypeAndPropagate(g, oldUse,
            newUsed.containerIn(g).get, newUsed.policy)
        }


      case (TypeConstructor, InstanceValue) =>
        redirectTypeConstructorToInstanceValueDecl(g, oldUse, newUsed)()

      case (StableValue, StableValue) =>
        redirect(g, oldUse, newUsed).map(_._1)
      case (TypeConstructor, StableValue) =>
        redirect(g, oldUse, newUsed).flatMap {
          case (g1, _) =>
            val AccessAbstraction(absNode, _) = newUsed
            updateTypedNodesInTypeConstraint(g1, oldUse, absNode)
        }

      case (kt1, kt2) =>
        LoggedError(s"redirection of type $kt1 to $kt2 unhandled")
    }

    val log = s"redirect uses and propagate from $oldKindType to $newKindType\n"
    log <++: ltg
  }

  def redirectSourceOfInitUseInFactory
  ( g: DependencyGraph,
    ctorDecl: NodeId,
    ctorDef : NodeId,
    initializer : NodeId,
    factory : NodeId) : DependencyGraph = {

    val clazz = g.container_!(ctorDecl)
    val factoryDef = g.definitionOf_!(factory)
    val selfUse = Uses(clazz,clazz)
    val g1 = g.changeSource(Uses(ctorDef, initializer), factoryDef)
      .addUses(factoryDef, clazz)
      .removeBinding(selfUse, (ctorDef, initializer))
      .addBinding((factoryDef, clazz), (factoryDef, initializer))

    val returnTypeConstraint = {
      val tucs =  g1.typeConstraints(factory).filter{
        case Sub(_, TypeOf(`factory`)) => true
        case _ => false
      }
      if(tucs.size > 1) error()
      tucs.head
    }
    //type constraint on factory return type
    ???
    //val Sub((_, st)) = returnTypeConstraint

    val g2 =  g1.removeTypeConstraint(returnTypeConstraint)
      .addTypeConstraint(Sub(TypeOf(factoryDef), TypeOf(factory)))
    //constraint on newly extracted local variable
    //      .addTypeConstraint((factoryDef, st), returnTypeConstraint)
    ???

    if(g2.typeMemberUsesOf(selfUse).isEmpty)
      g2.removeEdge(selfUse)
    else g2
  }

  def cl(g: DependencyGraph, u : NodeIdP) : Set[(NodeIdP, NodeIdP)] = {
    val tus : Set[NodeIdP] =
      g.kindType(u.used) match {
        case TypeDecl => Set(u)
        case _ => g.typeUsesOf(u)
      }

    for {
      tu <-  tus
      tmu <- g.typeMemberUsesOf(tu)
    } yield (tu, tmu)

  }



  def tmAbstraction
  (g : DependencyGraph,
   typeAbsId : NodeId,
   tmImplId : NodeId
  ) : LoggedTry[Abstraction] = {
    val log = s"tmAbstraction : searching an abstraction of ${(g, tmImplId).shows} in ${(g, typeAbsId).shows}\n"
    val absSet = g.abstractions(tmImplId).filter { abs =>
      abs.nodes.forall(g.contains(typeAbsId,_))
    }
    if(absSet.size != 1)
      LoggedError(log + s" one abstraction required ${absSet.size} found")
    else LoggedSuccess(absSet.head)
  }

  private [rules] def redirect
  (g : DependencyGraph,
   oldUse : NodeIdP,
   newUsed : Abstraction) : LoggedTry[(DependencyGraph, Set[NodeIdP])] =
    try LoggedSuccess {
      newUsed match {
        case AccessAbstraction(absId, _) =>
          (g.changeTarget(Uses(oldUse), absId), Set((oldUse.user, absId)))
        case _ => puck.error("redirect toward read / write abs should not be handled here")
      }
    } catch {
      case e : PuckError => LoggedError(e.getMessage)
    }




  //update type constraint when user change but type used remains the same
  def updateTypedNodesInTypeConstraint
  (g : DependencyGraph,
   ctorUse : NodeIdP,
   newUsed : NodeId,
   removeOld : Boolean = true) : LoggedTG = {
    val (userOfCtor, ctor)= ctorUse
    val ctorTypeUse = (ctor, g container_! ctor)

    val constraintsToUpdate =
      for{
        n <- g subTree userOfCtor
        tc <- g typeConstraints n
        if tc.typedNodes contains ctor
      } yield tc

//    println("oldUse = "+ (g, ctorUse).shows)
//    println("constraintsToUpdate = " +constraintsToUpdate)
    g.typ(newUsed) match {
      case NamedType(tid) =>
        constraintsToUpdate.foldLoggedEither(g){
          case (g0, btc @ BinaryTypeConstraint(op, Typed(_), Typed(_))) =>
            val newBtc = TypeConstraint.changeTyped(btc, ctor, newUsed)

//            println("old contraint = ")
//            println((g, btc).shows)
//            println("new contraint = ")
//            println((g, newBtc).shows)


            val g1 =
              if(removeOld) g0.removeTypeConstraint(btc)
              else g0

            LoggedSuccess(g1.addTypeConstraint(newBtc))
          case (g0 ,tc) =>
            LoggedError((g0,tc).shows + " type constraint change unhandled")
        }
      case _ => LoggedError("gen type factory type constraint change unhandled : TODO")
    }
  }

  def redirectTypeConstructorToInstanceValueDecl
  (g : DependencyGraph,
   ctorUse : NodeIdP,
   newUsed : Abstraction )
  ( createVarStrategy: CreateVarStrategy = CreateTypeMember(g.nodeKindKnowledge.defaultKindForNewReceiver)
  ) : LoggedTG = {
    val AccessAbstraction(absNode, _) = newUsed

    val g1 = g.changeTarget(Uses(ctorUse), absNode)

    import g.nodeKindKnowledge.intro

    val typeOfNewReveiver = g.container(absNode).get
    val (userOfCtor, _)= ctorUse

    val ltg = createVarStrategy match {
      case CreateParameter =>
        val decl = g1.getConcreteNode(g1.container_!(userOfCtor))

        intro.parameter(g1, typeOfNewReveiver, decl.id) map {
          case (pNode, g2) =>
            g2.addBinding((pNode.id, typeOfNewReveiver), (userOfCtor, absNode))
        }

      case CreateTypeMember(kind) =>
        intro.typeMember(g1, typeOfNewReveiver,
          g.hostTypeDecl(userOfCtor), kind) map {
          case (newTypeUse, g2) =>
            intro.addUsesAndSelfDependency(
              g2.addBinding(newTypeUse, (userOfCtor, absNode)),
              userOfCtor, newTypeUse.user)
        }
    }
    ltg flatMap (updateTypedNodesInTypeConstraint(_, ctorUse, absNode))


  }




  def propagateParameterTypeConstraints
  (g : DependencyGraph,
   typeMember : NodeId,
   absId : NodeId) : DependencyGraph = {

    val parameters = g parametersOf typeMember
    if(parameters.isEmpty) g
    else {
      val absParameters = g parametersOf absId
      val paramsConstraints = for {
        pids <- parameters zip absParameters
        (pid, absPid) = pids
        ct <- g.typeConstraints(pid)
      } yield (pid, absPid, ct)

      val nodeConstrainedByTypeMemberParameters =
        paramsConstraints.flatMap{
          case (_,_,ct) => ct.typedNodes.filterNot(parameters.contains)
        }

      val userOfTmAndConstrainedNodes = for {
        userOfTM <- g.usersOf(typeMember)
        constrainedNode <- nodeConstrainedByTypeMemberParameters
        if g.uses(userOfTM, constrainedNode)
      } yield userOfTM

      val removeOldTypeConstraint = userOfTmAndConstrainedNodes.isEmpty
      //      val removeOldTypeConstraint = g.usersOf(typeMember).intersect(g.usersOf(absId)).isEmpty

      paramsConstraints.foldLeft(g) {
        case (g01, (pid, absPid, tc)) =>
          val tc2 = Mapping.typeConstraint(id =>
            if(id == pid) absPid
            else id)(tc)


          if(!TypeConstraint.comply(g01, tc2))
            puck.error(s"${(g01, tc).shows} replaced by ${(g01, tc2).shows} : new type constraint does not comply")

          val g02 = g01.addTypeConstraint(tc2)

          if(removeOldTypeConstraint) g02.removeTypeConstraint(tc)
          else g02
      }
    }
  }

  def redirectTypeMemberUses
  ( g : DependencyGraph,
    oldTypeUses : NodeIdP,
    brSet : Set[(NodeIdP, NodeIdP)], // \forall p in brSet, p._1 == oldTypeUses
    newTypeToUse : NodeId,
    abstractionPolicy: AbstractionPolicy
  ): LoggedTG = {

    val getTMabs : (DependencyGraph, NodeId) => LoggedTry[NodeId] =
        abstractionPolicy match {
          case DelegationAbstraction =>
            (g, id) =>
              tmAbstraction(g, newTypeToUse, id) map (_.nodes.head)
          case SupertypeAbstraction =>

            val ltmap = Abstraction.typeMembersFirstOverriddenAbstraction(g,
              newTypeToUse,
              (brSet map (_._2._2)).toList)

            (g, id) =>
              ltmap flatMap  ( m => m get id match {
                case Some(absId) => LoggedSuccess(absId)
                case None => LoggedError("")
              })
        }
    val newTypeUse = (oldTypeUses.user, newTypeToUse)
    "redirectTypeMemberUses:\n" <++:
      brSet.foldLoggedEither(g) {
        case (g00, (_, tmu)) =>

          for {
            absId <- getTMabs(g, tmu.used)
            g01 <-
            LoggedSuccess(s"changing target of ${(g00, tmu).shows} for ${(g00, absId).shows(desambiguatedFullName)}",
              g00.changeTarget(Uses(tmu), absId))
            newTmu = (tmu.user, absId)
          } yield {
            val g02 = g01.removeBinding(oldTypeUses, tmu)
              .changeTypeUseOfTypeMemberUse(oldTypeUses, newTypeUse, newTmu)
            propagateParameterTypeConstraints(g02, tmu.used, absId)
          }
      }
  }

  def redirectFieldTowardGetterSetter // how much is this function general ?
  (g : DependencyGraph,
   oldUse : NodeIdP,
   newUsed : Abstraction
  ) : LoggedTG = {

    val ReadWriteAbstraction(srid, swid) = newUsed

    //retrieving type use of field
    def typeUse(nodeId: NodeId) : NodeIdP = (nodeId, Type.mainId(g typ nodeId))
    val oldTypeUse = typeUse(oldUse.used)
//    println("on entering :")
//    println( "oldTypeUse = "+ (g, oldTypeUse).shows)
//    println( "g1.typeConstraints( oldTypeUse ) = ")
//    g.typeConstraints( oldUse.used ).foreach {
//      tuc =>
//        println((g, tuc).shows)
//    }
//    println("*********************")


    val log = s"redirectUsesOfWritableNodeAndPropagate(g, oldUse = ${(g, oldUse).shows},  " +
      s"newUsed = ${(g, newUsed).shows})\n"

    g.typeUsesOf(oldUse).foldLoggedEither(g){
      (g0, tu) =>

        ((srid, swid, g0.usesAccessKind((tu, oldUse))) match {
          case (Some(rid), _, Some(Read)) =>
            LoggedSuccess((g0.changeTarget(Uses(oldUse), rid)
              .changeTypeMemberUseOfTypeUse(oldUse, (oldUse.user, rid), tu)
              .rmAccessKind((tu, oldUse)), rid))

          case (_, Some(wid), Some(Write)) =>
            LoggedSuccess((g0.changeTarget(Uses(oldUse), wid)
              .changeTypeMemberUseOfTypeUse(oldUse, (oldUse.user, wid), tu)
              .rmAccessKind((tu, oldUse)), wid))

          case (Some(rid), Some(wid), Some(RW)) =>
            //create write uses through abstraction, read uses will be created below
            updateTypedNodesInTypeConstraint(
              g0.splitUsesWithTargets(tu, oldUse, rid, wid),
              oldUse, wid, removeOld = false).map (g => (g, rid) )

          //these cases may happen with a previously existing abstraction that has been registered upon graph creation
          case (_, _, Some(RW)) =>
            LoggedError("Write uses, needs read and write abstraction, found : " + (g, newUsed).shows)
          case (_, None, Some(Write)) =>
            LoggedError("Write uses, needs a write abstraction, found : " + (g, newUsed).shows)
          case (None, _, Some(Read)) =>
            LoggedError("Read uses, needs a read abstraction, found : " + (g, newUsed).shows)


          case pb => puck.error("redirect field uses toward getter and or setter case not handled : " +pb)
        }) flatMap {
          case (g1, newUsedNode) =>
            updateTypedNodesInTypeConstraint(g1, oldUse, newUsedNode)
        }
    }
  }




  def redirectInstanceUsesTowardAbstractionInAnotherTypeAndPropagate
  (g : DependencyGraph,
   oldUse : NodeIdP,
   newTypeToUse : NodeId,
   abstractionPolicy: AbstractionPolicy
  ) : LoggedTG = {

    val log = s"redirect instance uses and propagate: oldUse = ${(g, oldUse).shows},  " +
      s"newTypeToUse = ${(g, newTypeToUse).shows})\n"


    lazy val qualifyingSet = cl(g, oldUse)
    val log2 = qualifyingSet map (n => (g,n).shows) mkString(" type members TRset = {", "\n", "}\n")

    val ltg : LoggedTG =
      if( qualifyingSet.nonEmpty ){
        val groupedByTypeUse = qualifyingSet.groupBy(_._1)

        groupedByTypeUse.toList.foldLoggedEither(g comment log) {
          case (g0, (tu, brSet)) =>
            //val g1 = g0.changeTarget(Uses(tu), newTypeToUse)
            val g1 = g0.changeType(tu, newTypeToUse)
            redirectTypeMemberUses(g1, tu, brSet, newTypeToUse, abstractionPolicy)

        }
      }
      else // on redirige un type
        g.kindType(oldUse.used) match {
          case TypeDecl =>
            //val g2 = oldUse.changeTarget(g, Uses, newTypeToUse)
            val g2 = g.changeType(oldUse, newTypeToUse)
            checkOrPropagateTypeConstraints(g2, oldUse.user, oldUse.used, newTypeToUse)
          case _ => LoggedError(s"${(g, oldUse).shows} empty qualyfing set, type redirection is expected")
        }

    (log + log2) <++: ltg
  }

  def checkOrPropagateTypeConstraints
  ( g : DependencyGraph,
    typed : NodeId,
    oldNamedTypeId : NodeId,
    newNamedTypeId : NodeId) : LoggedTG = {
    val tcs = g.typeConstraints(typed)
    val toPropagate = tcs.filterNot(TypeConstraint.comply(g, _))
    if(toPropagate.isEmpty) LoggedSuccess(g)
    else {

      def checkIfCanBePropagetedAndDoSo
      ( g : DependencyGraph,
        ct : TypeConstraint,
        typed1 : NodeId,
        typed2 : NodeId) : LoggedTG = {
        val t =
          if(typed == typed1) typed2
          else typed1

//        import ShowDG._
//        println("ct = " + (g, ct).shows)
//        println("typed = " + (g, typed).shows)
//        println("t = " + (g, t).shows)
//        println(s"change ${(g, oldNamedTypeId).shows} to ${(g, newNamedTypeId).shows}")


        val canBePropagated = {
          val g2 = g.changeType((t, oldNamedTypeId), newNamedTypeId)
//          println("typed = " + (g2, typed).shows)
//          println("t = " + (g2, t).shows)
//          println("ct = " + (g2, ct).shows)
          TypeConstraint.comply(g2, ct)
        }

        if(canBePropagated)
          redirectUsesAndPropagate(g, (t, oldNamedTypeId),
            AccessAbstraction(newNamedTypeId, SupertypeAbstraction))
        else LoggedError(s"${(g,ct).shows} cannot be propagated")
      }

      toPropagate.foldLoggedEither(g){
        case (g0, ct @ (BinaryTypeConstraint(op, Typed(typed1), Typed(typed2)))) =>
          checkIfCanBePropagetedAndDoSo(g0, ct, typed1, typed2)
        case (g0, ct) =>
//          import ShowDG._
//          println("***********************************************************")
//          println("ct = ")
//          (g0, ct).println
//          println(s"change ${(g0, oldNamedTypeId).shows} to ${(g0, newNamedTypeId).shows}")

          val ct2 = TypeConstraint.changeNamedType(ct, oldNamedTypeId, newNamedTypeId)
//          println("ct2 = ")
//
//          (g0, ct2).println

          if(ct2 == ct) LoggedError(s"propagation of ${(g0,ct).shows} not handled")
          else {
            val List(typed1, typed2) = ct.typedNodes
            val g1 = g0.removeTypeConstraint(ct).addTypeConstraint(ct2)
            checkIfCanBePropagetedAndDoSo(g1, ct2, typed1, typed2)
          }

      }
    }
  }

}
