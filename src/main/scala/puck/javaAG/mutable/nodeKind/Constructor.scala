package puck.javaAG.mutable.nodeKind

import puck.graph.mutable.HasType
import puck.graph.mutable.constraints.{AbstractionPolicy, DelegationAbstraction, SupertypeAbstraction}
import puck.graph.AGError
import puck.javaAG.mutable.MethodType

/**
 * Created by lorilan on 31/07/14.
 */

case class Constructor private[javaAG]() extends JavaNodeKind with HasType[JavaNodeKind, MethodType.T]{

  override val toString = "Constructor"

  def create() = JavaNodeKind.constructor(typ)

  var decl : AST.ConstructorDecl = _

  def canContain(k : JavaNodeKind) = false

  override def abstractionPolicies = List(DelegationAbstraction())

  def abstractKinds(p : AbstractionPolicy) = p match {
    case DelegationAbstraction() => List( JavaNodeKind.typedKind( () => new ConstructorMethod(), typ))
    case SupertypeAbstraction() => throw new AGError("Constructor cannot be abstracted by SuperType strategy")
  }

}
