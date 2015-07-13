package puck.graph.transformations.rules

import puck.PuckError
import puck.graph._
import puck.util.LoggedEither, LoggedEither._
import scalaz._, Scalaz._

object Redirection {

  def redirectUses(g : DependencyGraph,
                   oldEdge : DGEdge, newUsed : NodeId,
                   keepOldUse : Boolean) : DependencyGraph = {

    val g3 = if(keepOldUse) g.addUses(oldEdge.user, newUsed)
    else oldEdge.changeTarget(g, newUsed)

    g3.changeType(oldEdge.user, oldEdge.used, newUsed)
  }


  def cl(g: DependencyGraph, u : DGUses) : Set[(DGUses, DGUses)] =
    for{
      tu <- g.typeUsesOf(u) + u
      tmu <- g.typeMemberUsesOf(tu)
    } yield (tu, tmu)


  def tmAbstraction
  ( g : DependencyGraph,
    typeAbs : NodeId,
    tmImpl : NodeId
    ) : LoggedTry[Abstraction] = {
    val absSet = g.abstractions(tmImpl).filter { abs =>
      abs.nodes.forall(g.contains(typeAbs,_))
    }
    if(absSet.size != 1) LoggedError(new PuckError(), s"one abstraction required ${absSet.size} found")
    else LoggedSuccess(absSet.head)
  }

  private def redirect(g : DependencyGraph,
                oldUse : DGUses,
                newUsed : Abstraction) : LoggedTry[(DependencyGraph, List[DGUses])] =
    try LoggedSuccess {
      (newUsed, oldUse.accessKind) match {
        case (AccessAbstraction(absId, pol), None) =>
          (oldUse.changeTarget(g, absId), List(oldUse.copy(target = absId)))
        case (ReadWriteAbstraction(Some(rid), _), Some(Read)) =>
          (oldUse.changeTarget(g, rid), List(oldUse.copy(target = rid)))
        case (ReadWriteAbstraction(_, Some(wid)), Some(Write)) =>
          (oldUse.changeTarget(g, wid), List(oldUse.copy(target = wid)))
        case (ReadWriteAbstraction(Some(rid), Some(wid)), Some(RW)) => ???
          //cannot "just" redirect, need to replace the accessKind
//          val g1 = oldUse.changeTarget(g, rid)
//          oldUse.changeTarget(g1, wid)
        case _ => throw new PuckError(s"error while redirecting $oldUse toward $newUsed")
      }
    } catch {
      case e : PuckError => LoggedError(e, e.getMessage)
    }


  def redirectUsesAndPropagate
    ( g : DependencyGraph,
      oldUse : DGUses,
      newUsed : Abstraction
      ): LoggedTG = {

    (g.kindType(oldUse.used), newUsed.kindType(g)) match {
      case (InstanceValueDecl, InstanceValueDecl)
           | (TypeDecl, TypeDecl) => redirectInstanceUsesAndPropagate(g,oldUse, newUsed)
      case (TypeConstructor, InstanceValueDecl) =>
        redirectTypeConstructorToInstanceValueDecl(g, oldUse, newUsed)
      case (StaticValueDecl, StaticValueDecl)
        | (TypeConstructor, StaticValueDecl) =>
         redirect(g, oldUse, newUsed).map(_._1)
      case (kt1, kt2) => LoggedError(new PuckError(), s"redirection of type $kt1 to $kt2 unhandled")
    }

  }

  def redirectTypeConstructorToInstanceValueDecl
  ( g : DependencyGraph,
    oldUse : DGUses,
    newUsed : Abstraction,
    createVarStrategy: CreateVarStrategy = CreateParameter
    ) : LoggedTG = {
    newUsed.nodes match {
      case List(absNode) =>

        val g1 = oldUse.changeTarget(g, absNode)

        val typeOfNewReveiver = g.container(absNode).get
        createVarStrategy match {
          case CreateParameter =>
            Move.createParam(g1, Uses(-1,-1), typeOfNewReveiver, Set(oldUse))

          case CreateTypeMember(kind) =>
            val user = oldUse.user

            Move.createTypeMember(g1,  Uses(-1,-1),
              typeOfNewReveiver,
              g.container(user).get,
              Set(oldUse),
              kind)
        }

      case _ => LoggedError(new PuckError(), "constructor should have one abs node")
    }
  }




  def redirectInstanceUsesAndPropagate
    ( g : DependencyGraph,
      oldUse : DGUses,
      newUsed : Abstraction
      ) : LoggedTG = {
    val log0 = s"redirectUsesAndPropagate(_, oldUse = $oldUse, " + s"newUsed = $newUsed)"
    val log1 =  s"\n$oldUse.exists = ${oldUse.existsIn(g)}\n"

    val newTypeToUse = newUsed.kindType(g) match {
      case TypeDecl =>
        val AccessAbstraction(nid, _) = newUsed
        nid
      case _ =>  newUsed.containerIn(g).get
    }

    val typeMemberTRset = cl(g, oldUse).filter{case (tu, _) => !tu.selfUse }


    val ltg = typeMemberTRset.groupBy(_._1).toList.foldLoggedEither(g comment log0) {
      case (g0, (tu, trSet)) =>
        val g1 = tu.changeTarget(g0, newTypeToUse)
        val newTypeUse = tu.copy(target = newTypeToUse)
        trSet.foldLoggedEither( g1 ) {
          case (g00, (_, tmu)) =>
            for{
              abs <- tmAbstraction(g, newTypeToUse, tmu.used)
              gNewTmus <- redirect(g00, tmu, abs)
              (g01, nTmus) = gNewTmus
            } yield {
              nTmus.foldLeft(g01.removeUsesDependency(tu,tmu)){
                _.addUsesDependency(newTypeUse,_)
              }
            }

        }
    }

    log0 <++: log1 <++: ltg
  }

//  def redirectTypeUsesOfTypeMemberUse(g : DependencyGraph,
//                                      currentTypeMemberUse : DGUses,
//                                      newTypeMemberUsed : NodeId,
//                                      policy : RedirectionPolicy) : LoggedTG = {
//
//    val log = s"redirecting Type uses of typeMember use ${showDG[DGEdge](g).shows(currentTypeMemberUse)}" +
//      s" (new typeMember used is ${showDG[NodeId](g).shows(newTypeMemberUsed)})\n"
//    val lg = g logComment log
//
//    val typeUses = g.typeUsesOf(currentTypeMemberUse)
//    if(typeUses.isEmpty)
//      (lg :++> "no primary uses to redirect\n").toLoggedEither
//    else{
//      val lg1 =
//        lg :++> "uses to redirect:%s".format(typeUses.mkString("\n\t", "\n\t","\n"))
//
//      val currentTypeUsed = typeUses.head.used
//      assert(typeUses.tail.forall(_.used == currentTypeUsed))
//
//      for {
//        newTypeUsed <- findNewTypeUsed(lg.value, currentTypeUsed, newTypeMemberUsed, policy)
//
//        g3 <- typeUses.foldLoggedEither[PuckError, DependencyGraph](lg){
//          (g0, typeUse) =>
//
//            val keepOldUse = g.typeMemberUsesOf(typeUse).tail.nonEmpty //is empty if typeUses had only one side use
//
//            val g1 = g.removeUsesDependency(typeUse, currentTypeMemberUse)
//
//            for {
//              g2 <-
//                if(typeUse.selfUse)  ??? //redirectThisTypeUse(g, typeUses.used, newTypeMemberUsed)
//                else redirectUsesAndPropagate(g, typeUse, newTypeUsed, policy, keepOldUse)
//            } yield
//                g2.addUsesDependency(Uses(typeUse.user, newTypeUsed),
//                  Uses(currentTypeMemberUse.user, newTypeMemberUsed))
//        }
//      } yield g3
//
//    }
//  }
//
//
//  def findNewTypeUsed
//  ( g : DependencyGraph,
//    currentTypeUsed : NodeId,
//    newTypeMemberUsed : NodeId,
//    policy : RedirectionPolicy
//    ) : LoggedTry[NodeId] = {
//
//    val log = s"searching new Type used ($policy) : current type used is " +
//      s"${showDG[NodeId](g).shows(currentTypeUsed)}, new typeMember used : ${showDG[NodeId](g).shows(newTypeMemberUsed)}\n"
//
//
//    def logFind(newPrimaryUsed : NodeId) =
//    LoggedSuccess(newPrimaryUsed, log +
//      s"new type to use found : ${showDG[NodeId](g).shows(newPrimaryUsed)}\n")
//
//
//    policy match {
//        case NotAnAbstraction => // used with merge rule
//          logFind(g.container(newTypeMemberUsed).get)
//        case absPolicy : AbstractionPolicy =>
//          g.abstractions(currentTypeUsed).find {
//            case AccessAbstraction(node, `absPolicy`) => g.contains_*(node, newTypeMemberUsed)
//            case _ => false
//          } match {
//            case Some(AccessAbstraction(n, _)) => logFind(n)
//            case Some(_) => sys.error("cannot happen")
//            case None => // Error ?? abstraction should be registred ?
//
//              val abstractKinds =
//                g.getNode(currentTypeUsed).kind.
//                  abstractionNodeKinds(absPolicy)
//
//              g.nodesId.find{ node =>
//                g.contains_*(node, newTypeMemberUsed) &&
//                  abstractKinds.contains(g.getNode(node).kind)
//
//              } match {
//                case Some(newPrimaryUsed) =>
//                  logFind(newPrimaryUsed)
//                case None =>
//                  LoggedError(new RedirectionError("no correct primary abstraction found !"))
//              }
//          }
//      }
//
//    }
//
//
//
//
//  type TypeMemberUses = List[DGUses]
//  def redirectTypeMemberUsesOfTypeUse
//    (g : DependencyGraph,
//     currentTypeUse: DGUses,
//     newTypeUsed : NodeId,
//     policy : RedirectionPolicy,
//     tmu : TypeMemberUses) : LoggedTG = {
//
//    val log = s"redirecting typeMember uses of type use ${showDG[DGEdge](g).shows(currentTypeUse)} " +
//      s"(new type used is  ${showDG[NodeId](g).shows(newTypeUsed)})"
//
//    val log1 = if (tmu.isEmpty) "no typeMember uses to redirect\n"
//    else "uses to redirect:%s".format(tmu.mkString("\n\t", "\n\t", "\n"))
//
//    val lg : LoggedG = (g logComment log) :++> ("\n" + log1)
//
//    tmu.foldLoggedEither[PuckError, DependencyGraph](lg) {
//      case (g0 : DependencyGraph, typeMemberUse) =>
//        val typeMember = typeMemberUse.used
//        val someTypeMemberAbs =
//          g.abstractions(typeMember).find { abs =>
//            assert(abs.nodes.nonEmpty)
//            abs.nodes.forall(g.contains(newTypeUsed, _))
//          }
//
//        someTypeMemberAbs match {
//          case None =>
//            val msg = s"While redirecting type use ${showDG[DGEdge](g).shows(currentTypeUse)} " +
//              s"target to ${showDG[NodeId](g).shows(newTypeUsed)}\n" +
//              s"no satisfying abstraction to redirect typeMember use ${showDG[DGEdge](g).shows(typeMemberUse)}"
//
//            g0.set(msg+"\n").toLoggedEither[PuckError].error(new RedirectionError(msg))
//
//          case Some(abs) =>
//            redirectUsesAndPropagate(
//              g0.removeUsesDependency(currentTypeUse, typeMemberUse),
//              typeMemberUse, abs,
//              keepOldUse = false).map {
//                g2 =>
//                  abs.nodes.foldLeft(g2){
//                    (g0, absId) =>
//                      val newSide = Uses(typeMemberUse.user, absId)
//                      g2.addUsesDependency(Uses(currentTypeUse.user, newTypeUsed), newSide)
//                  }
//
//
//              }
//        }
//    }
//
//  }
//
//
//
//  type KeepOldTypeUse = Boolean
//  def redirectTypeMemberAndConstructorUsesOfTypeUse
//  ( g : DependencyGraph,
//    currentTypeUse: DGUses,
//    newTypeUsed : NodeId,
//    policy : RedirectionPolicy
//    ): LoggedEither[PuckError, (KeepOldTypeUse, DependencyGraph)] = {
//
//    val log = s"redirecting typeMember AND CONSTRUCTOR uses of type use ${showDG[DGEdge](g).shows(currentTypeUse)} " +
//      s"(new type used is  ${showDG[NodeId](g).shows(newTypeUsed)})"
//    val lg = (g logComment log) :++> "\n"
//
//
//    val typeMemberAndTypeCtorUses = g.typeMemberUsesOf(currentTypeUse).toList
//
//    import puck.util.Collections.SelectList
//
//    def newTypeUsedHasAccessAbstractionOf(nId : NodeId) : Boolean =
//      g.abstractions(nId).exists {
//        case AccessAbstraction(abs, _) => g.contains(newTypeUsed, abs)
//        case _ => false
//      }
//
//    val redirect : DependencyGraph => LoggedEither[PuckError, (KeepOldTypeUse, DependencyGraph)] =
//    typeMemberAndTypeCtorUses.select { e => g.kindType(e.target) == TypeConstructor} match {
//      case Some((typeCtorUse, typeMemberUses))
//        if ! newTypeUsedHasAccessAbstractionOf(typeCtorUse.used) =>
//        redirectTypeMemberUsesOfTypeUse(_, currentTypeUse, newTypeUsed, policy, typeMemberUses).map((true, _))
//      case _ =>
//        redirectTypeMemberUsesOfTypeUse(_, currentTypeUse, newTypeUsed, policy, typeMemberAndTypeCtorUses).map((false, _))
//    }
//
//    lg.toLoggedEither[PuckError] flatMap redirect
//  }
}
