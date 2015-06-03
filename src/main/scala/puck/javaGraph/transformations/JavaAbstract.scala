package puck.javaGraph
package transformations

import nodeKind._
import puck.graph.constraints.{SupertypeAbstraction, AbstractionPolicy}
import puck.graph._
import puck.graph.transformations.rules.Abstract

import scalaz._

object JavaAbstract extends Abstract {

  override def absIntroPredicate( graph : DependencyGraph,
                                  impl : DGNode,
                                  absPolicy : AbstractionPolicy,
                                  absKind : NodeKind) : NodePredicateT = {
    (impl.kind, absPolicy) match {
      case (Method, SupertypeAbstraction)
           | (AbstractMethod, SupertypeAbstraction) =>
        (graph, potentialHost) => !graph.interloperOf(graph.container(impl.id).get, potentialHost.id)
      case _ => super.absIntroPredicate(graph, impl, absPolicy, absKind)
    }
  }

  override def abstractionName( g: DependencyGraph, impl: ConcreteNode, abskind : NodeKind, policy : AbstractionPolicy) : String = {
    if (impl.kind == Constructor)
      "create"
    else
      (abskind, policy) match {
        case (Method, SupertypeAbstraction)
             | (AbstractMethod, SupertypeAbstraction) => impl.name
        case _ => super.abstractionName(g, impl, abskind, policy)

      }
  }

  override def createAbstraction
  ( g : DependencyGraph,
    impl: ConcreteNode,
    abskind : NodeKind ,
    policy : AbstractionPolicy
    ) : LoggedTry[(ConcreteNode, DependencyGraph)] = {
    (abskind, policy) match {
      case (Interface, SupertypeAbstraction) =>
        val methods = typeMembersToPutInInterface(g, impl, SupertypeAbstraction)
        abstractTypeDeclAndReplaceByAbstractionWherePossible(g,
          impl,
          Interface, SupertypeAbstraction,
          methods)

      case (AbstractMethod, SupertypeAbstraction) =>
        //no (abs, impl) or (impl, abs) uses
        LoggedSuccess(createAbsNode(g, impl, abskind, policy))

      case (ConstructorMethod, _) =>
        super.createAbstraction(g, impl, abskind, policy) map { case (abs, g0) =>
          (abs, addTypesUses(g0, abs))
        }

      case _ => super.createAbstraction(g, impl, abskind, policy)
    }
  }

  override def abstractionCreationPostTreatment
  ( g: DependencyGraph,
    implId : NodeId,
    absId : NodeId,
    policy : AbstractionPolicy
    ) : DependencyGraph = {
    val abstraction = g.getNode(absId)
    (abstraction.kind, policy) match {
      case (AbstractMethod, SupertypeAbstraction) =>
        val implContainer = g.container(implId).get
        val thisClassNeedsImplement = !g.abstractions(implContainer).exists
          {case (abs, absPolicy) => absPolicy == SupertypeAbstraction &&
            abs == g.container(absId).get}

        if(!thisClassNeedsImplement) g
        else {
          val absContainer = g.container(absId).get
          val g1 = g.addUses(implContainer, absContainer)
            .addIsa(implContainer, absContainer)

          g1.content(absId).foldLeft(g1){
            case (g0, absMethodId) => val absMeth = g0.getConcreteNode(absMethodId)
              g0.changeType(absMethodId, absMeth.styp, implId, absId)
          }
        }
      case _ => g
    }
  }
}
