package puck.graph
package transformations.rules

import puck.PuckError
import puck.graph.constraints.{AbstractionPolicy, SupertypeAbstraction, DelegationAbstraction}
import puck.util.LoggedEither._

import scalaz.std.set._
import scalaz.std.list._
import scalaz.syntax.writer._

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
    sUsesAccessKind: Option[UsesAccessKind]) : Option[Type] =
    sUsesAccessKind match {
      case None => impl.styp
      case Some(Read) => impl.styp map (Arrow(???, _))
      case Some(Write) => impl.styp map (Arrow(_, ???))
      case Some(RW) => sys.error("should not happen")
  }

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

    val g1 = g.changeContravariantType(meth.id, meth.styp, clazz.id, interface.id)

    if(g1.uses(meth.id, clazz.id))
      g1.set(s"changeSelfTypeUseBySuperInTypeMember : redirecting ${DGEdge.UsesK(meth.id, clazz.id)} target to $interface\n")
        .toLoggedEither.flatMap {
        Redirection.redirectUsesAndPropagate(_, DGEdge.UsesK(meth.id, clazz.id),
          AccessAbstraction(interface.id, SupertypeAbstraction),
          propagateRedirection = true, keepOldUse = false)
      }
    else LoggedSuccess(g1)
  }


  def canBeAbstracted
    (g : DependencyGraph,
     member : ConcreteNode,
     clazz : ConcreteNode,
     policy : AbstractionPolicy) : Boolean = {
    //the originSibling arg is needed in case of cyclic uses
    def aux(originSibling : ConcreteNode)(member: ConcreteNode): Boolean = {

      def sibling: NodeId => Boolean =
        sid => g.contains(clazz.id, sid) && sid != originSibling.id && sid != member.id

      def usedOnlyViaSelf(user : NodeId, used : NodeId) : Boolean = {
        val typeUses = g.typeUsesOf(user, used)
        typeUses.forall { _.selfUse }
      }

      //TODO check if the right part of the and is valid for Delegation abstraction
      member.kind.canBeAbstractedWith(policy) && {
        val usedNodes = g.usedBy(member.id)
        usedNodes.isEmpty || {
          val usedSiblings = usedNodes filter sibling map g.getConcreteNode
          usedSiblings.forall {
            used0 =>
              aux(member)(used0) || usedOnlyViaSelf(member.id, used0.id)
          }
        }
      }
    }

    aux(member)(member)
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

  def addTypesUses(g : DependencyGraph, nodeId : NodeId) : DependencyGraph =
    g.getConcreteNode(nodeId).styp.map(_.ids) match {
      case None => g
      case Some(typesUsed) =>
        typesUsed.foldLeft(g){(g0, tid) => g0.addUses(nodeId, tid)}
    }

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
          g0.kindType(child) match {
            case TypeMember =>
              changeSelfTypeUseBySuperInTypeMember(g0, child, clazz, interface)
            case _ => LoggedSuccess(g0)
          }
        }
      else LoggedSuccess[DependencyGraph](g3)

      g5 <- logInterfaceCreation(g4, interface)
    } yield {
      (interfaceAbs, g5)
    }
  }

  def logInterfaceCreation(g : DependencyGraph, itc : ConcreteNode) : LoggedTG = {
    import ShowDG._
    val log = s"interface $itc created, contains : {" +
      g.content(itc.id).map(showDG[NodeId](g).show).mkString("\n")+
      "}"
    LoggedSuccess(g, log)
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



  //SO SO UGLY !!!
  def abstractionCreationPostTreatment
  ( g : DependencyGraph,
    implId : NodeId,
    absId : NodeId,
    policy : AbstractionPolicy
    ) : DependencyGraph = g




}
