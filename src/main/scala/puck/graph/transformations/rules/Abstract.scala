package puck.graph
package transformations.rules

import constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.util.Collections._

import scalaz.\/-

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
    ) : Try[DependencyGraph] =
    traverse(g.directSuperTypes(subTypeId), g){
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

  def changeSelfTypeUseBySuperInTypeMember(g : DependencyGraph, meth : ConcreteNode,
                                             clazz : ConcreteNode, interface : ConcreteNode): Try[DependencyGraph] ={

    val g1 = g.changeContravariantType(meth.id, meth.styp, clazz.id, interface.id)

    if(g1.uses(meth.id, clazz.id)) {
      g.logger.writeln(s"interface creation : redirecting ${DGEdge.UsesK(meth.id, clazz.id)} target to $interface")
      Redirection.redirectUsesAndPropagate(g1, DGEdge.UsesK(meth.id, clazz.id), interface.id, SupertypeAbstraction)
    }
    else \/-(g1)
  }

  def typeMembersToPutInInterface
  ( g : DependencyGraph,
    clazz : ConcreteNode,
    policy : AbstractionPolicy) : Seq[ConcreteNode] = {

    def canBeAbstracted(member : ConcreteNode) : Boolean = {
      //the originSibling arg is needed in case of cyclic uses
      def aux(originSibling : ConcreteNode)(member: ConcreteNode): Boolean = {

        def sibling: NodeId => Boolean =
          sid => g.contains(clazz.id, sid) && sid != originSibling.id

          member.kind.canBeAbstractedWith(policy) && {

          val usedNodes = g.usedBy(member.id)

          usedNodes.isEmpty || {
            val usedSiblings = usedNodes filter sibling
            usedSiblings.map(g.getConcreteNode).forall {
              used0 => aux(member)(used0) || {
                val typeUses = g.typeUsesOf(member.id, used0.id)
                typeUses.forall { _.selfUse }
              }
            }
          }
        }
      }
      aux(member)(member)
    }

    g.content(clazz.id).foldLeft(Seq[ConcreteNode]()){
      (acc, mid) =>
        val member = g.getConcreteNode(mid)
        if(canBeAbstracted(member)) member +: acc
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
    ) : Try[DependencyGraph] ={

    createAbstraction(g, meth, meth.kind.abstractKinds(policy).head,  policy) map {
      case (absMethod, g0) =>
        g0.addContains(interface.id, absMethod.id)
        //is change type needed in case of delegation policy
        .changeType(absMethod.id, absMethod.styp, clazz.id, interface.id)
    }
  }


  def abstractTypeDeclAndReplaceByAbstractionWherePossible
  ( g : DependencyGraph,
    clazz : ConcreteNode,
    abskind : NodeKind,
    policy : AbstractionPolicy,
    members : Seq[ConcreteNode]
    ) : Try[(ConcreteNode, DependencyGraph)] = {
    for{
      itcGraph <- createAbsNodeAndUse(g, clazz, abskind, policy)
      (interface, g1) = itcGraph

      g2 <- traverse(members, g1){ (g0, member) =>
        createAbstractTypeMember(g0, member, clazz, interface, policy)
      }

      g3 <- insertInTypeHierarchy(g2, clazz.id, interface.id)

      g4 <- if(policy == SupertypeAbstraction)
        traverse(members, g3.addIsa(clazz.id, interface.id)){ (g0, child) =>
          g0.kindType(child) match {
            case TypeMember =>
              changeSelfTypeUseBySuperInTypeMember(g0, child, clazz, interface)
            case _ => \/-(g0)
          }
        }
      else \/-(g3)
    } yield {
      logInterfaceCreation(g4, interface)
      (interface, g4)
    }
  }

  def logInterfaceCreation(g : DependencyGraph, itc : ConcreteNode) : Unit = {
    import ShowDG._
    g.logger.writeln(s"interface $itc created, contains : {")
    g.logger.writeln(g.content(itc.id).map(showDG[NodeId](g).show).mkString("\n"))
    g.logger.writeln("}")
  }

  private def createAbsNodeAndUse(g : DependencyGraph,
                        impl: ConcreteNode,
                        abskind : NodeKind ,
                        policy : AbstractionPolicy) : Try[(ConcreteNode, DependencyGraph)] = {
    val (abs, g1) = createAbsNode(g, impl, abskind, policy)

    \/-((abs, policy match {
      case SupertypeAbstraction => g1.addUses(impl.id, abs.id)
      case DelegationAbstraction => g1.addUses(abs.id, impl.id)
    }))

  }

  def createAbstraction(g : DependencyGraph,
                        impl: ConcreteNode,
                        abskind : NodeKind ,
                        policy : AbstractionPolicy) : Try[(ConcreteNode, DependencyGraph)] =
    createAbsNodeAndUse(g,impl, abskind, policy)

  //SO SO UGLY !!!
  def abstractionCreationPostTreatment
  ( g : DependencyGraph,
    implId : NodeId,
    absId : NodeId,
    policy : AbstractionPolicy
    ) : DependencyGraph = g




}
