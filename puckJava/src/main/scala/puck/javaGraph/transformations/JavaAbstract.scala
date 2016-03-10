package puck.javaGraph
package transformations

import nodeKind._
import puck.graph._
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.graph.transformations.rules.Abstract

object JavaAbstract extends Abstract {

  implicit class MyStringOps(val str: String) extends AnyVal {
    def upFirst =
      str.length match {
        case 0 => str
        case 1 => str.charAt(0).toUpper.toString
        case _ => str.charAt(0).toUpper + str.substring(1)
      }

  }

  override def abstractionName
  (g: DependencyGraph,
   impl: ConcreteNode,
   abskind: NodeKind,
   policy: AbstractionPolicy,
   sUsesAccessKind: Option[UsesAccessKind]
    ): String =
    (impl.kind, abskind, policy, sUsesAccessKind) match {
      case (Constructor, _, _, _) => "create"
      case (Field, Method, DelegationAbstraction, Some(Read)) => "get" + impl.name.upFirst
      case (Field, Method, DelegationAbstraction, Some(Write)) => "set" + impl.name.upFirst
      case (_, Method, SupertypeAbstraction, _)
           | (_, AbstractMethod, SupertypeAbstraction, _) => impl.name
      case _ => super.abstractionName(g, impl, abskind, policy, sUsesAccessKind)

    }

}