package puck.graph.transformations.rules

import puck.graph.DependencyGraph.Mutability
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.graph._

import scalaz.\/-

trait Intro {

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

  def createNode
  ( graph : DependencyGraph,
    localName : String,
    kind : NodeKind,
    th : Option[Type],
    mutable : Mutability = true
    ) : (ConcreteNode, DependencyGraph) =
  graph.addConcreteNode(localName, kind, th, mutable)


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
  def abstractionCreationPostTreatment(g : DependencyGraph,
                                       implId : NodeId,
                                       absId : NodeId,
                                       policy : AbstractionPolicy) : DependencyGraph = g




}
