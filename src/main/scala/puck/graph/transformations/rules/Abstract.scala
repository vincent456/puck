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

  def abstractTypeDecl
   ( g : DependencyGraph,
     impl: ConcreteNode,
     abskind : NodeKind,
     policy : AbstractionPolicy,
     typeMembersToAbstract : Seq[ConcreteNode]) = {

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

  //SO SO UGLY !!!
  def abstractionCreationPostTreatment
  ( g : DependencyGraph,
    implId : NodeId,
    absId : NodeId,
    policy : AbstractionPolicy
    ) : DependencyGraph = g




}
