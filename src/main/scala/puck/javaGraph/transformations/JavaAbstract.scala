package puck.javaGraph
package transformations

import nodeKind._
import puck.graph.constraints._
import puck.graph._
import puck.graph.transformations.rules.Abstract

object JavaAbstract extends Abstract {



  override def absIntroPredicate( impl : DGNode,
                                  absPolicy : AbstractionPolicy,
                                  absKind : NodeKind) : NodePredicateT = {
    (impl.kind, absPolicy) match {
      case (Method, SupertypeAbstraction)
           | (AbstractMethod, SupertypeAbstraction) =>
        (graph, potentialHost) => !graph.interloperOf(graph.container(impl.id).get, potentialHost.id)
      case _ => super.absIntroPredicate(impl, absPolicy, absKind)
    }
  }

  override def absType
  ( g : DependencyGraph,
    impl : ConcreteNode,
    sUsesAccessKind: Option[UsesAccessKind]) : Option[Type] = {

    val voidId = {
      val sNode = g.concreteNodes.find(_.name == "void")
      if(sNode.isEmpty) sys.error("void not loaded")
      else sNode.get.id
    }

    (sUsesAccessKind, impl.styp) match {
      case (Some(Read), Some(t@NamedType(_))) =>
        Some(MethodType(Tuple(), t))
      case (Some(Write), Some(t)) =>
        Some(MethodType(Tuple(List(t)), NamedType(voidId)))
      case _ => super.absType(g, impl, sUsesAccessKind)
    }
  }
  override def abstractionName
  ( g: DependencyGraph,
    impl: ConcreteNode,
    abskind : NodeKind,
    policy : AbstractionPolicy,
    sUsesAccessKind: Option[UsesAccessKind]
    ) : String = {
    if (impl.kind == Constructor)
      "create"
    else
      (abskind, policy) match {
        case (Method, SupertypeAbstraction)
             | (AbstractMethod, SupertypeAbstraction) => impl.name
        case _ => super.abstractionName(g, impl, abskind, policy, sUsesAccessKind)

      }
  }

  override def createAbstraction
  ( g : DependencyGraph,
    impl: ConcreteNode,
    abskind : NodeKind ,
    policy : AbstractionPolicy
    ) : LoggedTry[(Abstraction, DependencyGraph)] = {


    (abskind, policy) match {
      case (Interface, SupertypeAbstraction)
      | (Class, SupertypeAbstraction) =>
        val methods = typeMembersToPutInInterface(g, impl, SupertypeAbstraction)
        LoggedSuccess(g, s"Creating $abskind with abstractions of" +
          methods.mkString("\n", "\n", "\n")).flatMap {
          abstractTypeDeclAndReplaceByAbstractionWherePossible(_,
            impl,
            abskind, SupertypeAbstraction,
            methods)
        }
      case (Class, DelegationAbstraction) =>

        val methods = g.content(impl.id).foldLeft(List[ConcreteNode]()){
          (acc, mid) =>
            val member = g.getConcreteNode(mid)

            if(member.kind.canBeAbstractedWith(DelegationAbstraction)) member +: acc
            else acc
        }

        abstractTypeDeclAndReplaceByAbstractionWherePossible(g,
          impl,
          Class, DelegationAbstraction,
          methods)

      case (AbstractMethod, SupertypeAbstraction) =>
        //no (abs, impl) or (impl, abs) uses
        LoggedSuccess(createAbsNode(g, impl, abskind, policy))

      case (ConstructorMethod, _) =>
        super.createAbstraction(g, impl, abskind, policy) map {
          case (abs @ AccessAbstraction(absId,_), g0) => (abs, addTypesUses(g0, absId))
          case _ => sys.error("should not happen")
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
        val thisClassNeedsImplement =
          !g.abstractions(implContainer).exists{
          case AccessAbstraction(abs, SupertypeAbstraction) =>
            abs == g.container(absId).get
           case _ => false
        }

        if(!thisClassNeedsImplement) g
        else {
          val absContainer = g.container(absId).get
          val g1 = g.addUses(implContainer, absContainer)
            .addIsa(implContainer, absContainer)

          g1.content(absId).foldLeft(g1){
            case (g0, absMethodId) =>
              g0.changeType(absMethodId, implId, absId)
          }
        }
      case _ => g
    }
  }
}
