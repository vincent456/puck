package puck.javaGraph
package transformations

import nodeKind._
import puck.graph._
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.graph.transformations.rules.Abstract
import puck.javaGraph.transformations

object JavaAbstract extends Abstract {



  override def absIntroPredicate( impl : DGNode,
                                  absPolicy : AbstractionPolicy,
                                  absKind : NodeKind) : NodePredicateT = {
    (impl.kind, absPolicy) match {
      case (Method, SupertypeAbstraction)
           | (AbstractMethod, SupertypeAbstraction) =>
        (graph, potentialHost) => {
          val typeDecl = graph.container(impl.id).get
          val potentialSuperType = potentialHost.id
          val canExtends = !graph.interloperOf(typeDecl, potentialSuperType)
          canExtends
        }
      case _ => super.absIntroPredicate(impl, absPolicy, absKind)
    }
  }

  implicit class MyStringOps(val str : String) extends AnyVal{
    def upFirst =
      str.length match {
        case 0 => str
        case 1 => str.charAt(0).toUpper.toString
        case _ => str.charAt(0).toUpper + str.substring(1)
      }

  }

  override def abstractionName
  ( g: DependencyGraph,
    impl: ConcreteNode,
    abskind : NodeKind,
    policy : AbstractionPolicy,
    sUsesAccessKind: Option[UsesAccessKind]
    ) : String =

    (impl.kind, abskind, policy, sUsesAccessKind) match {
        case (Constructor, _, _, _) => "create"
        case (Field, Method, DelegationAbstraction, Some(Read)) => "get"+ impl.name.upFirst
        case (Field, Method, DelegationAbstraction, Some(Write)) => "set"+ impl.name.upFirst
        case (_, Method, SupertypeAbstraction, _)
             | (_, AbstractMethod, SupertypeAbstraction,_) => impl.name
        case _ => super.abstractionName(g, impl, abskind, policy, sUsesAccessKind)

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
        val log = s"Creating $abskind with abstractions of" +
                 methods.mkString("\n", "\n", "\n")

        val ltg = abstractTypeDeclAndReplaceByAbstractionWherePossible(g,
            impl,
            abskind, SupertypeAbstraction,
            methods)

        log <++: ltg

      case (Class, DelegationAbstraction) =>

        val methods = g.content(impl.id).foldLeft(List[ConcreteNode]()){
          (acc, mid) =>
            val member = g.getConcreteNode(mid)

            if(member.kind.canBeAbstractedWith(DelegationAbstraction)) member +: acc
            else acc
        }

        abstractTypeDeclAndReplaceByAbstractionWherePossible(g,
          impl, Class, DelegationAbstraction, methods)


      case (AbstractMethod, SupertypeAbstraction) =>
        //no (abs, impl) or (impl, abs) uses
        LoggedSuccess(createAbsNode(g, impl, abskind, policy))

      case (ConstructorMethod, _) =>
        super.createAbstraction(g, impl, abskind, policy) map {
          case (abs @ AccessAbstraction(absId,_), g0) => (abs, ???) //addTypesUses(g0, absId))
          case _ => sys.error("should not happen")
        }

      case _ => super.createAbstraction(g, impl, abskind, policy)
    }
  }

}
