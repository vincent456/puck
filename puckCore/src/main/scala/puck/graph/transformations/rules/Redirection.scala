package puck.graph.transformations.rules

import puck.{graph, PuckError}
import puck.graph._
import puck.util.LoggedEither, LoggedEither._
import puck.graph.ShowDG._
import scalaz.std.list._
import scalaz.std.set._

object Redirection {

  def redirectSourceOfInitUseInFactory
  ( g: DependencyGraph,
    ctorDecl: NodeId,
    ctorDef : NodeId,
    initializer : NodeId,
    factory : NodeId) : DependencyGraph = {

    val clazz = g.container_!(ctorDecl)
    val factoryDef = g.definitionOf_!(factory)
    g.changeSource(g.getUsesEdge_!(ctorDef, initializer), factoryDef)
      .addUses(factoryDef, clazz)
      .removeUsesDependency((clazz,clazz), (ctorDef, initializer))
      .addUsesDependency((factoryDef, clazz), (factoryDef, initializer))
  }

  def cl(g: DependencyGraph, u : DGUses) : Set[(DGUses, DGUses)] = {
    val cl0 =  for {
      tu <- g.typeUsesOf(u) + u
      tmu <- g.typeMemberUsesOf(tu)
    } yield (tu, tmu)

    val cl1 = for  {
      tmu <- g.typeMemberUsesOf(u) + u
      tu <- g.typeUsesOf(tmu)
    } yield (tu, tmu)

    cl0 ++ cl1
  }


  def tmAbstraction
  ( g : DependencyGraph,
    typeAbs : NodeId,
    tmImpl : NodeId
    ) : LoggedTry[Abstraction] = {
    val absSet = g.abstractions(tmImpl).filter { abs =>
      abs.nodes.forall(g.contains(typeAbs,_))
    }
    if(absSet.size != 1) LoggedError(s"one abstraction required ${absSet.size} found")
    else LoggedSuccess(absSet.head)
  }

  private [rules] def redirect
  ( g : DependencyGraph,
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
      case e : PuckError => LoggedError(e.getMessage)
    }


  def redirectUsesAndPropagate
    ( graph : DependencyGraph,
      oldUse : DGUses,
      newUsed : Abstraction
      ): LoggedTG = {

    val g = graph.comment(s"Redirection.redirectUsesAndPropagate(g, ${(graph,oldUse).shows}, $newUsed)")

    val oldKindType = g.kindType(oldUse.used)
    val newKindType = newUsed.kindType(g)

    val ltg : LoggedTG = (oldKindType, newKindType) match {
      case (InstanceValueDecl, InstanceValueDecl)
           | (TypeDecl, TypeDecl) =>
        redirectInstanceUsesAndPropagate(g, oldUse, newUsed)

      case (TypeConstructor, InstanceValueDecl) =>
        redirectTypeConstructorToInstanceValueDecl(g, oldUse, newUsed)()

      case (StaticValueDecl, StaticValueDecl)
        | (TypeConstructor, StaticValueDecl) =>
         redirect(g, oldUse, newUsed).map(_._1)

      case (kt1, kt2) =>
        LoggedError(s"redirection of type $kt1 to $kt2 unhandled")
    }

    val log = s"redirect uses and propagate from $oldKindType to $newKindType\n"
    log <++: ltg
  }

  def redirectTypeConstructorToInstanceValueDecl
  ( g : DependencyGraph,
    oldUse : DGUses,
    newUsed : Abstraction )
  ( createVarStrategy: CreateVarStrategy = CreateTypeMember(g.nodeKindKnowledge.defaultKindForNewReceiver)
    ) : LoggedTG = {
    newUsed.nodes match {
      case List(absNode) =>

        val g1 = oldUse.changeTarget(g, absNode)

        import g.nodeKindKnowledge.intro
        import DGEdge.toPair

        val typeOfNewReveiver = g.container(absNode).get
        val userOfCtor = oldUse.user

        createVarStrategy match {
          case CreateParameter =>
            val decl = g1.getConcreteNode(g1.container_!(userOfCtor))

            intro.parameter(g1, typeOfNewReveiver, decl.id) map {
              case (pNode, g2) =>
                g2.addUsesDependency((pNode.id, typeOfNewReveiver), (userOfCtor, absNode))
            }

          case CreateTypeMember(kind) =>
            intro.typeMember(g1,
              typeOfNewReveiver,
              g.hostTypeDecl(userOfCtor),
              kind) map {
              case (newTypeUse, g2) =>
                intro.addUsesAndSelfDependency(
                  g2.addUsesDependency(newTypeUse, (userOfCtor, absNode)),
                  userOfCtor, newTypeUse.user)
            }
        }

      case _ => LoggedError("constructor should have one abs node")
    }
  }

  def redirectInstanceUsesAndPropagate
    ( g : DependencyGraph,
      oldUse : DGUses,
      newUsed : Abstraction
      ) : LoggedTG = {
    val log = s"redirectUsesAndPropagate(_, oldUse = $oldUse, " + s"newUsed = $newUsed)\n"

    val newTypeToUse = newUsed.kindType(g) match {
      case TypeDecl =>
        val AccessAbstraction(nid, _) = newUsed
        nid
      case _ => newUsed.containerIn(g).get
    }

    val typeMemberTRset = cl(g, oldUse) //.filter{ case (tu, _) => !tu.selfUse }

    val ltg : LoggedTG =
      if(typeMemberTRset.nonEmpty)
        typeMemberTRset.groupBy(_._1).toList.foldLoggedEither(g comment log) {
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
                  _.changeTypeUseOfTypeMemberUse(tu, newTypeUse,_)
                }
              }
          }
        }
      else {
        if(newUsed.kindType(g) == TypeDecl)
          redirect(g, oldUse, newUsed).map(_._1)
        else
          LoggedError("empty binding relationship and not redirecting a type decl")
      }

    log <++: ltg
  }


}
