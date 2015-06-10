package puck.graph
package transformations.rules

import constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.util.LoggedEither._

import scalaz._, Scalaz._

abstract class Abstract {


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
        Redirection.redirectUsesAndPropagate(_, DGEdge.UsesK(meth.id, clazz.id), interface.id, SupertypeAbstraction)
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
        sid => g.contains(clazz.id, sid) && sid != originSibling.id

      def usedByOnlyViaSelf(user : NodeId, used : NodeId) : Boolean = {
        val typeUses = g.typeUsesOf(user, used)
        typeUses.forall { _.selfUse }
      }

      //TODO check if the right part of the and is valid for Delegation abstraction
      member.kind.canBeAbstractedWith(policy) && {

        val usedNodes = g.usedBy(member.id)

        usedNodes.isEmpty || {
          val usedSiblings = usedNodes filter sibling
          usedSiblings.map(g.getConcreteNode).forall {
            used0 => aux(member)(used0) ||
              usedByOnlyViaSelf(member.id, used0.id)
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

  def addTypesUses(g : DependencyGraph, node : ConcreteNode) : DependencyGraph =
    node.styp.map(_.ids) match {
      case None => g
      case Some(typesUsed) =>
        typesUsed.foldLeft(g){(g0, tid) => g0.addUses(node.id, tid)}
    }

  def createAbstractTypeMember
  ( g : DependencyGraph,
    meth : ConcreteNode,
    clazz : ConcreteNode,
    interface : ConcreteNode,
    policy : AbstractionPolicy
    ) : LoggedTG ={


    try {
      createAbstraction(g, meth, meth.kind.abstractKinds(policy).head, policy) map {
        case (absMethod, g0) =>
          g0.addContains(interface.id, absMethod.id)
            //is change type needed in case of delegation policy
            .changeType(absMethod.id, absMethod.styp, clazz.id, interface.id)
      }
    } catch {
      case t : Throwable=>
        println("createAbstractTypeMember( g : DependencyGraph,\n"+
          s"$meth : ConcreteNode,\n"+
          s"$clazz : ConcreteNode,\n"+
            s"$interface : ConcreteNode,\n"+
            s"$policy : AbstractionPolicy)")
        throw t
    }
  }


  def abstractTypeDeclAndReplaceByAbstractionWherePossible
  ( g : DependencyGraph,
    clazz : ConcreteNode,
    abskind : NodeKind,
    policy : AbstractionPolicy,
    members : List[ConcreteNode]
    ) : LoggedTry[(ConcreteNode, DependencyGraph)] = {
    for{
      itcGraph <- createAbsNodeAndUse(g, clazz, abskind, policy)
      (interface, g1) = itcGraph

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
      (interface, g5)
    }
  }

  def logInterfaceCreation(g : DependencyGraph, itc : ConcreteNode) : LoggedTG = {
    import ShowDG._
    val log = s"interface $itc created, contains : {" +
      g.content(itc.id).map(showDG[NodeId](g).show).mkString("\n")+
      "}"
    LoggedSuccess(g, log)
  }

  private def createAbsNodeAndUse(g : DependencyGraph,
                        impl: ConcreteNode,
                        abskind : NodeKind ,
                        policy : AbstractionPolicy) : LoggedTry[(ConcreteNode, DependencyGraph)] = {
    val (abs, g1) = createAbsNode(g, impl, abskind, policy)

    LoggedSuccess((abs, policy match {
      case SupertypeAbstraction => g1.addUses(impl.id, abs.id)
      case DelegationAbstraction => g1.addUses(abs.id, impl.id)
    }))

  }

  def createAbstraction(g : DependencyGraph,
                        impl: ConcreteNode,
                        abskind : NodeKind ,
                        policy : AbstractionPolicy) : LoggedTry[(ConcreteNode, DependencyGraph)] =
    createAbsNodeAndUse(g, impl, abskind, policy)

  //SO SO UGLY !!!
  def abstractionCreationPostTreatment
  ( g : DependencyGraph,
    implId : NodeId,
    absId : NodeId,
    policy : AbstractionPolicy
    ) : DependencyGraph = g




}
