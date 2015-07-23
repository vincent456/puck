package puck.graph
package transformations.rules

import puck.PuckError
import puck.graph.ShowDG._
import puck.graph.constraints.{AbstractionPolicy, SupertypeAbstraction, DelegationAbstraction}
import puck.util.LoggedEither._

import scalaz.std.set._
import scalaz.std.list._

abstract class Abstract {


  def absIntroPredicate(impl : DGNode,
                        absPolicy : AbstractionPolicy,
                        absKind : NodeKind) : NodePredicateT = absPolicy match {
    case SupertypeAbstraction =>
      (graph, potentialHost) => !graph.interloperOf(impl.id, potentialHost.id)

    case DelegationAbstraction =>
      (graph, potentialHost) => !graph.interloperOf(potentialHost.id, impl.id)
  }



  def abstractionName
  ( g: DependencyGraph,
    impl: ConcreteNode,
    abskind : NodeKind,
    policy : AbstractionPolicy,
    sUsesAccessKind: Option[UsesAccessKind]
    ) : String =
    impl.name + "_" + policy

  def absType
  ( g : DependencyGraph,
    impl : ConcreteNode,
    sUsesAccessKind: Option[UsesAccessKind]) : Option[Type] = ???
//    sUsesAccessKind match {
//      case None => impl.styp
//      case Some(Read) => impl.styp map (Arrow(???, _))
//      case Some(Write) => impl.styp map (Arrow(_, ???))
//      case Some(RW) => sys.error("should not happen")
//  }

  def createAbsNode
  ( g : DependencyGraph,
    impl: ConcreteNode,
    abskind : NodeKind,
    policy : AbstractionPolicy
    ) : (Abstraction, DependencyGraph) =
    policy match {
      case DelegationAbstraction if impl.kind.canBeReadOrWrote =>

        val rName = abstractionName(g, impl, abskind, DelegationAbstraction, Some(Read))
        val rStype = absType(g, impl, Some(Read))
        val wName = abstractionName(g, impl, abskind, DelegationAbstraction, Some(Write))
        val wStype = absType(g, impl, Some(Write))
        val (rNode, g1) = g.addConcreteNode(rName, abskind, rStype)
        val (wNode, g2) = g1.addConcreteNode(wName, abskind, wStype)
        val abs = ReadWriteAbstraction(Some(rNode.id), Some(wNode.id))
        (abs, g2.addAbstraction(impl.id, abs))

      case _ =>

        val name = abstractionName(g, impl, abskind, policy, None)
        val (n, g1) = g.addConcreteNode(name, abskind, absType(g, impl, None))
        g1.parameters(impl.id).foldLeft(g1){
          (g0, paramId) =>
            val param = g0.getConcreteNode(paramId)
            val (pabs, g01) = g0.addConcreteNode(param.name, param.kind, g0.styp(paramId), mutable = true)
            g01.usedBy(paramId).foldLeft(g01.addParam(n.id, pabs.id)){
              (g00, tid) => g01.addUses(pabs.id, tid)
            }
        }
        val abs = AccessAbstraction(n.id, policy)
        (abs, g1.addAbstraction(impl.id, abs))

    }




  def insertInTypeHierarchy
  ( g : DependencyGraph,
    subTypeId : NodeId,
    newSuperTypeId : NodeId
    ) : LoggedTG =
    g.directSuperTypes(subTypeId).foldLoggedEither(g){
      (g0, oldSuperTypedId) =>

        val g1 = g0.changeSource(Isa(subTypeId, oldSuperTypedId), newSuperTypeId)
          .changeSource(Uses(subTypeId, oldSuperTypedId), newSuperTypeId)
        val subTypeMeths = g1.content(subTypeId).toList.map(g1.getConcreteNode)
        val newSupTypeMeths = g1. content(newSuperTypeId).toList.map(g1.getConcreteNode)
        val oldSupTypeMeths = g1. content(oldSuperTypedId).toList.map(g1.getConcreteNode)

        Type.findAndRegisterOverridedInList(g1, newSupTypeMeths, subTypeMeths){
          Type.ignoreOnImplemNotFound
        } flatMap (
          Type.findAndRegisterOverridedInList(_, oldSupTypeMeths, newSupTypeMeths){
            Type.ignoreOnImplemNotFound
          })

    }

  def changeSelfTypeUseBySuperInTypeMember
  ( g : DependencyGraph, meth : ConcreteNode,
    clazz : ConcreteNode, interface : ConcreteNode): LoggedTG ={

      val g1 = g.changeContravariantType(meth.id, g.styp(meth.id), clazz.id, interface.id)
      val log = "changeSelfTypeUseBySuperInTypeMember : " +
        s"redirecting Uses(${meth.name}, ${clazz.name}) target to $interface\n"

      val ltg = g.parameters(meth.id).foldLoggedEither(g1){
        (g0, pid) =>
          if (g0.uses(pid, clazz.id))
            Redirection.redirectUsesAndPropagate(g0, Uses(pid, clazz.id),
              AccessAbstraction(interface.id, SupertypeAbstraction))
          else
            LoggedSuccess(g0)

      }
      log <++: ltg

  }


  def canBeAbstracted
    (g : DependencyGraph,
     memberDecl : ConcreteNode,
     clazz : ConcreteNode,
     policy : AbstractionPolicy) : Boolean = {
    //the originSiblingDecl arg is needed in case of cyclic uses
    def aux(originSiblingDecl : ConcreteNode)(memberDecl: ConcreteNode): Boolean = {


      def sibling: NodeId => Boolean =
        sid => g.contains(clazz.id, sid) &&
          sid != originSiblingDecl.id &&
          sid != memberDecl.id
//          g.hostTypeDecl(sid) == clazz.id && {
//
//          val sDecl = sid.kind.kindType match {
//            case ValueDef => g container_! sid.id
//            case _ => sid.id
//          }
//
//          sDecl != originSiblingDecl.id &&
//            sDecl != memberDecl.id
//        }


      def usedOnlyViaSelf(user : NodeId, used : NodeId) : Boolean = {
        val typeUses = g.typeUsesOf(user, used)
        typeUses.forall { _.selfUse }
      }

      //TODO check if the right part of the and is valid for Delegation abstraction
      memberDecl.kind.canBeAbstractedWith(policy) && {

        val usedNodes = (memberDecl.definition(g) map g.usedBy).getOrElse(Set())

        usedNodes.isEmpty || {
          val usedSiblings = usedNodes filter sibling map g.getConcreteNode
          usedSiblings.forall {
            used0 =>
              aux(memberDecl)(used0) || usedOnlyViaSelf(memberDecl.definition_!(g), used0.id)
          }
        }
      }
    }

    aux(memberDecl)(memberDecl)
  }


  def typeMembersToPutInInterface
  ( g : DependencyGraph,
    clazz : ConcreteNode,
    policy : AbstractionPolicy) : List[ConcreteNode] = {


    g.content(clazz.id).foldLeft(List[ConcreteNode]()){
      (acc, mid) =>
        val member = g.getConcreteNode(mid)
        if(canBeAbstracted(g, member, clazz, policy)) member +: acc
        else acc
    }
  }

//  def addTypesUses(g : DependencyGraph, nodeId : NodeId) : DependencyGraph =
//    g.getConcreteNode(nodeId).styp.map(_.ids) match {
//      case None => g
//      case Some(typesUsed) =>
//        typesUsed.foldLeft(g){(g0, tid) => g0.addUses(nodeId, tid)}
//    }


  def createAbstractTypeMember
  ( g : DependencyGraph,
    meth : ConcreteNode,
    clazz : ConcreteNode,
    interface : ConcreteNode,
    absKind : AbstractionPolicy
    ) : LoggedTG ={

//    try {
      createAbstraction(g, meth, meth.kind.abstractionNodeKinds(absKind).head, absKind) flatMap {
        case (AccessAbstraction(absMethodId, _), g0) =>

          LoggedSuccess(g0.addContains(interface.id, absMethodId)
            //is change type needed in case of delegation policy
            .changeType(absMethodId, clazz.id, interface.id))
        case _ => LoggedError(new PuckError("unexpected type of abstraction"))
      }
//    } catch {
//      case t : Throwable=>
//        println("createAbstractTypeMember( g : DependencyGraph,\n"+
//          s"$meth : ConcreteNode,\n"+
//          s"$clazz : ConcreteNode,\n"+
//            s"$interface : ConcreteNode,\n"+
//            s"$absKind : AbstractionPolicy)")
//        throw t
//    }
  }


  def abstractTypeDeclAndReplaceByAbstractionWherePossible
  ( g : DependencyGraph,
    clazz : ConcreteNode,
    abskind : NodeKind,
    policy : AbstractionPolicy,
    members : List[ConcreteNode]
    ) : LoggedTry[(Abstraction, DependencyGraph)] = {
    for{
      itcGraph <- createAbsNodeAndUse(g, clazz, abskind, policy)

      (interfaceAbs, g1) = itcGraph
      interface = interfaceAbs match {
        case AccessAbstraction(id, _) => g1.getConcreteNode(id)
        case _ => sys.error("type should not have a RW abstraction")
      }

      g2 <- members.foldLoggedEither(g1){ (g0, member) =>
        createAbstractTypeMember(g0, member, clazz, interface, policy)
      }

      g3 <- policy match {
        case SupertypeAbstraction => insertInTypeHierarchy(g2, clazz.id, interface.id)
        case DelegationAbstraction => LoggedSuccess(g2)
      }

      g4 <- if(policy == SupertypeAbstraction)
        members.foldLoggedEither(g3.addIsa(clazz.id, interface.id)){
          (g0, child) =>
            child.kind.kindType match {
            case InstanceValueDecl if g0.parameters(child.id).exists(g0.uses(_, clazz.id)) =>
              changeSelfTypeUseBySuperInTypeMember(g0, child, clazz, interface)
            case _ => LoggedSuccess(g0)
          }
        }
      else LoggedSuccess[DependencyGraph](g3)


      log = s"interface $interface created, contains : {" +
             g4.content(interface.id).map(showDG[NodeId](g4).show).mkString("\n")+
             "}"
      g5 <- LoggedSuccess(g4, log)

    } yield {
      (interfaceAbs, g5)
    }
  }


  private def createAbsNodeAndUse
  ( g : DependencyGraph,
    impl: ConcreteNode,
    abskind : NodeKind ,
    policy : AbstractionPolicy
    ) : LoggedTry[(Abstraction, DependencyGraph)] = {
    val (abs, g1) = createAbsNode(g, impl, abskind, policy)

    LoggedSuccess((abs, abs match {
      case AccessAbstraction(absId, SupertypeAbstraction) => g1.addUses(impl.id, absId)
      case AccessAbstraction(absId, DelegationAbstraction) => g1.addUses(absId, impl.id)
      case rwAbs @ ReadWriteAbstraction(someRid, someWid) =>
        val g2 = someRid.map(g1.addUses(_, impl.id, Some(Read))).getOrElse(g1)
          someWid.map(g2.addUses(_, impl.id, Some(Write))).getOrElse(g2)

    }))

  }

  def createAbstraction
  ( g : DependencyGraph,
    impl: ConcreteNode,
    abskind : NodeKind ,
    policy : AbstractionPolicy
    ) : LoggedTry[(Abstraction, DependencyGraph)] = {
    createAbsNodeAndUse(g, impl, abskind, policy)
  }



}
