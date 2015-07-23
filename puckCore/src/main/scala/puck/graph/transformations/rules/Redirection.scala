package puck.graph.transformations.rules

import puck.graph.constraints.SupertypeAbstraction
import puck.{graph, PuckError}
import puck.graph._
import puck.util.LoggedEither, LoggedEither._
import scalaz._, Scalaz._

object Redirection {

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

    val oldKindType = g.kindType(oldUse.used)
    val newKindType = newUsed.kindType(g)

    val ltg : LoggedTG = (oldKindType, newKindType) match {
      case (InstanceValueDecl, InstanceValueDecl)
           | (TypeDecl, TypeDecl) =>
        redirectInstanceUsesAndPropagate(g, oldUse, newUsed)

      case (TypeConstructor, InstanceValueDecl) =>
        redirectTypeConstructorToInstanceValueDecl(g, oldUse, newUsed)

      case (StaticValueDecl, StaticValueDecl)
        | (TypeConstructor, StaticValueDecl) =>
         redirect(g, oldUse, newUsed).map(_._1)

      case (kt1, kt2) =>
        LoggedError(new PuckError(), s"redirection of type $kt1 to $kt2 unhandled")
    }

    val log = s"redirect uses and propagate from $oldKindType to $newKindType\n"
    log <++: ltg
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
    val log = s"redirectUsesAndPropagate(_, oldUse = $oldUse, " + s"newUsed = $newUsed)\n"

    val newTypeToUse = newUsed.kindType(g) match {
      case TypeDecl =>
        val AccessAbstraction(nid, _) = newUsed
        nid
      case _ => newUsed.containerIn(g).get
    }

    val typeMemberTRset = cl(g, oldUse).filter{ case (tu, _) => !tu.selfUse }

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
                  _.addUsesDependency(newTypeUse,_)
                }
              }
          }
        }
      else {
        if(newUsed.kindType(g) == TypeDecl)
          redirect(g, oldUse, newUsed).map(_._1)
        else
          LoggedError(new graph.Error(), "empty binding relationship and not redirecting a type decl")
      }

    log <++: ltg
  }


}
